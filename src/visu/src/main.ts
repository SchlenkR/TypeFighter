import { TreeVisualizer } from './TreeVisualizer';
import type { JsNode } from './types';
import html2canvas from 'html2canvas';
// @ts-ignore - gif.js doesn't have proper TypeScript types
import GIF from 'gif.js';

let treeViz: TreeVisualizer;
let runButtons: HTMLButtonElement[] = [];
let currentRunIndex = 0;

function getRunLabel(jsNode: JsNode, index: number): string {
  // Just use the number (1-indexed)
  return `${index + 1}`;
}

function selectRun(index: number): void {
  if (!treeViz) return;

  // Remove any existing highlights before changing the run
  document.querySelectorAll('.tvar.highlight').forEach(el => {
    el.classList.remove('highlight');
  });

  const selectFirstTVarCheckbox = document.getElementById('select-first-tvar') as HTMLInputElement;
  const shouldReselectFirstTVar = selectFirstTVarCheckbox ? selectFirstTVarCheckbox.checked : false;

  const treesForSolverRuns = window.treesForSolverRuns;
  if (index < 0 || index >= treesForSolverRuns.length) return;
  
  const nextRun = treesForSolverRuns[index];
  if (!nextRun) return;

  currentRunIndex = index;

  runButtons.forEach((button, buttonIndex) => {
    button.classList.toggle('active', buttonIndex === index);
  });

  treeViz.loadRun(nextRun, index);

  if (shouldReselectFirstTVar) {
    // Re-apply the selection after the new run has been loaded
    treeViz.toggleSelectFirstTVar(true);
  }

  // Re-apply constraint hiding after run change
  const constraintHideInput = document.getElementById('constraint-hide-input') as HTMLInputElement;
  if (constraintHideInput && treeViz) {
    const text = constraintHideInput.value.trim();
    if (text !== '') {
      const indices = text
        .split(/[\s,]+/)
        .map(s => s.trim())
        .filter(s => s !== '')
        .map(s => parseInt(s, 10))
        .filter(n => !isNaN(n) && n >= 0);
      
      setTimeout(() => treeViz.setHiddenConstraintIndices(indices), 50);
    }
  }
}

function nextRun(): void {
  const treesForSolverRuns = window.treesForSolverRuns;
  if (treesForSolverRuns.length === 0) return;
  
  // Wrap around to the beginning if at the end
  const nextIndex = (currentRunIndex + 1) % treesForSolverRuns.length;
  selectRun(nextIndex);
}

function previousRun(): void {
  const treesForSolverRuns = window.treesForSolverRuns;
  if (treesForSolverRuns.length === 0) return;
  
  // Wrap around to the end if at the beginning
  const prevIndex = currentRunIndex === 0 ? treesForSolverRuns.length - 1 : currentRunIndex - 1;
  selectRun(prevIndex);
}

function setupRunButtons(runs: JsNode[]): void {
  const buttonContainer = document.getElementById('run-buttons')!;
  runButtons = [];
  buttonContainer.innerHTML = '';

  if (!runs.length) {
    const emptyState = document.createElement('div');
    emptyState.className = 'run-buttons-empty';
    emptyState.textContent = 'No AST instances available.';
    buttonContainer.appendChild(emptyState);
    return;
  }

  runs.forEach((run, index) => {
    const button = document.createElement('button');
    button.type = 'button';
    button.className = 'run-button';
    button.textContent = getRunLabel(run, index);
    button.addEventListener('click', () => selectRun(index));
    runButtons.push(button);
    buttonContainer.appendChild(button);
  });

  selectRun(0);
}

// Initialize when page loads
window.addEventListener('load', () => {
  const treesForSolverRuns = window.treesForSolverRuns || [];
  treeViz = new TreeVisualizer(treesForSolverRuns, 0);
  setupRunButtons(treesForSolverRuns);
  setupGifCreation(treesForSolverRuns);
  setupPngExport(treesForSolverRuns);
  setupScreenshot();
  setupDownloadAll();
  setupClearDownloads();
  setupControlPanel();

  // Expose selectRun globally for solver panel buttons
  (window as any).selectRunFromPanel = selectRun;

  // Add keyboard navigation for solver runs
  window.addEventListener('keydown', (event) => {
    if (event.key === 'ArrowLeft') {
      previousRun();
      event.preventDefault();
    } else if (event.key === 'ArrowRight') {
      nextRun();
      event.preventDefault();
    }
  });
});

function setupControlPanel(): void {
  const envPanelToggle = document.getElementById('env-panel-toggle') as HTMLInputElement;
  
  if (envPanelToggle) {
    // Set initial state
    if (treeViz) {
      treeViz.toggleEnvPanel(envPanelToggle.checked);
    }

    envPanelToggle.addEventListener('change', () => {
      if (treeViz) {
        treeViz.toggleEnvPanel(envPanelToggle.checked);
      }
    });
  }

  const solverRunPanelToggle = document.getElementById('solver-run-panel-toggle') as HTMLInputElement;
  
  if (solverRunPanelToggle) {
    // Set initial state
    if (treeViz) {
      treeViz.toggleSolverRunPanel(solverRunPanelToggle.checked);
    }

    solverRunPanelToggle.addEventListener('change', () => {
      if (treeViz) {
        treeViz.toggleSolverRunPanel(solverRunPanelToggle.checked);
      }
    });
  }

  const inputPanelToggle = document.getElementById('code-panel-toggle') as HTMLInputElement;
  
  if (inputPanelToggle) {
    // Set initial state
    if (treeViz) {
      treeViz.toggleInputPanel(inputPanelToggle.checked);
    }

    inputPanelToggle.addEventListener('change', () => {
      if (treeViz) {
        treeViz.toggleInputPanel(inputPanelToggle.checked);
      }
    });
  }

  const tracePanelToggle = document.getElementById('trace-panel-toggle') as HTMLInputElement;
  
  if (tracePanelToggle) {
    // Set initial state
    if (treeViz) {
      treeViz.toggleTracePanel(tracePanelToggle.checked);
    }

    tracePanelToggle.addEventListener('change', () => {
      if (treeViz) {
        treeViz.toggleTracePanel(tracePanelToggle.checked);
      }
    });
  }

  const selectFirstTVarCheckbox = document.getElementById('select-first-tvar') as HTMLInputElement;
  if (selectFirstTVarCheckbox) {
    selectFirstTVarCheckbox.addEventListener('change', () => {
      if (treeViz) {
        treeViz.toggleSelectFirstTVar(selectFirstTVarCheckbox.checked);
      }
    });
  }

  const showTVarsToggle = document.getElementById('show-tvars-toggle') as HTMLInputElement;
  if (showTVarsToggle) {
    showTVarsToggle.addEventListener('change', () => {
      if (treeViz) {
        treeViz.toggleTVarsVisibility(showTVarsToggle.checked);
      }
    });
  }

  // Wire up constraint visibility control with text input
  const constraintHideInput = document.getElementById('constraint-hide-input') as HTMLInputElement;

  if (constraintHideInput) {
    constraintHideInput.addEventListener('input', () => {
      if (!treeViz) return;
      
      const text = constraintHideInput.value.trim();
      if (text === '') {
        // Show all constraints
        treeViz.setHiddenConstraintIndices([]);
      } else {
        // Parse the input: split by space or comma, convert to numbers
        const indices = text
          .split(/[\s,]+/)
          .map(s => s.trim())
          .filter(s => s !== '')
          .map(s => parseInt(s, 10))
          .filter(n => !isNaN(n) && n >= 0);
        
        treeViz.setHiddenConstraintIndices(indices);
      }
    });
  }

  // Wire up control panel textarea to update input panel
  const controlInputTextarea = document.getElementById('control-code-text') as HTMLTextAreaElement;
  if (controlInputTextarea) {
    controlInputTextarea.addEventListener('input', () => {
      if (treeViz) {
        // Always mirror text into CODE panel
        treeViz.updateInputText(controlInputTextarea.value);

        // Auto-open CODE panel if it's hidden so the user immediately sees the text reflected
        const inputToggle = document.getElementById('code-panel-toggle') as HTMLInputElement | null;
        if (inputToggle && !inputToggle.checked) {
          inputToggle.checked = true;
          treeViz.toggleInputPanel(true);
        }
      }
    });

    // On load, if there is prefilled text, sync it into CODE panel as well
    if (controlInputTextarea.value && treeViz) {
      treeViz.updateInputText(controlInputTextarea.value);
    }
  }
}

function setupGifCreation(runs: JsNode[]): void {
  const createButton = document.getElementById('gif-create-button') as HTMLButtonElement;
  const runsInput = document.getElementById('gif-runs-input') as HTMLInputElement;
  const durationInput = document.getElementById('gif-duration-input') as HTMLInputElement;

  createButton.addEventListener('click', () => {
    const runsText = runsInput.value.trim();
    let runNumbers: number[];

    if (!runsText) {
      // If no numbers entered, use all solver runs
      runNumbers = Array.from({ length: runs.length }, (_, i) => i + 1);
    } else {
      runNumbers = runsText
        .split(/\s+/)
        .map(s => parseInt(s, 10))
        .filter(n => !isNaN(n) && n >= 1 && n <= runs.length);

      if (runNumbers.length === 0) {
        alert('Please enter valid run numbers (1-' + runs.length + ')');
        return;
      }
    }

    const duration = parseInt(durationInput.value, 10);
    if (isNaN(duration) || duration < 100) {
      alert('Please enter a valid duration (minimum 100ms)');
      return;
    }

    createGif(runNumbers, duration);
  });
}

async function createGif(runNumbers: number[], duration: number): Promise<void> {
  const createButton = document.getElementById('gif-create-button') as HTMLButtonElement;
  const containerInner = document.getElementById('container-inner');
  
  if (!containerInner) {
    alert('Container not found');
    return;
  }

  // Disable button and show progress
  createButton.disabled = true;
  createButton.textContent = 'Creating GIF...';

  try {
    // Initialize GIF encoder
    const gif = new GIF({
      workers: 2,
      quality: 5, // Better quality (1-30, lower = better but slower)
      workerScript: '/gif.worker.js'
    });

    // Capture frames for each specified run
    for (let i = 0; i < runNumbers.length; i++) {
      const runIndex = runNumbers[i] - 1; // Convert to 0-based index
      
      // Update button to show progress
      createButton.textContent = `Capturing frame ${i + 1}/${runNumbers.length}...`;
      
      // Switch to the specified run
      selectRun(runIndex);
      
      // Wait a bit for the render to complete
      await new Promise(resolve => setTimeout(resolve, 100));
      
      // Capture the container-inner with its background
      const canvas = await html2canvas(containerInner, {
        backgroundColor: null, // Use the element's own background
        scale: 2, // Capture at 2x resolution for better quality
        logging: false
      });
      
      // Add frame to GIF
      gif.addFrame(canvas, { delay: duration });
    }

    // Show encoding progress
    createButton.textContent = 'Encoding GIF...';

    // Render the GIF
    gif.on('finished', (blob: Blob) => {
      // Create filename with timestamp
      const timestamp = new Date().toISOString().replace(/[:.]/g, '-').slice(0, -5);
      const filename = `solver-runs-animation-${timestamp}.gif`;
      
      // Add download link to the downloads section
      addDownloadLink(blob, filename, '🎬');
      
      // Also auto-download
      const url = URL.createObjectURL(blob);
      const link = document.createElement('a');
      link.download = filename;
      link.href = url;
      link.click();
      
      // Cleanup
      setTimeout(() => URL.revokeObjectURL(url), 100);
      
      // Reset button
      createButton.disabled = false;
      createButton.textContent = 'Create GIF';
    });

    gif.on('progress', (progress: number) => {
      createButton.textContent = `Encoding: ${Math.round(progress * 100)}%`;
    });

    gif.render();

  } catch (error) {
    console.error('Error creating GIF:', error);
    alert('Error creating GIF: ' + (error instanceof Error ? error.message : String(error)));
    
    // Reset button
    createButton.disabled = false;
    createButton.textContent = 'Create GIF';
  }
}

function setupPngExport(runs: JsNode[]): void {
  const exportButton = document.getElementById('export-pngs-button') as HTMLButtonElement;
  const runsInput = document.getElementById('gif-runs-input') as HTMLInputElement;

  exportButton.addEventListener('click', () => {
    const runsText = runsInput.value.trim();
    let runNumbers: number[];

    if (!runsText) {
      // If no numbers entered, use all solver runs
      runNumbers = Array.from({ length: runs.length }, (_, i) => i + 1);
    } else {
      runNumbers = runsText
        .split(/\s+/)
        .map(s => parseInt(s, 10))
        .filter(n => !isNaN(n) && n >= 1 && n <= runs.length);

      if (runNumbers.length === 0) {
        alert('Please enter valid run numbers (1-' + runs.length + ')');
        return;
      }
    }

    exportPngs(runNumbers);
  });
}

async function exportPngs(runNumbers: number[]): Promise<void> {
  const exportButton = document.getElementById('export-pngs-button') as HTMLButtonElement;
  const containerInner = document.getElementById('container-inner');
  
  if (!containerInner) {
    alert('Container not found');
    return;
  }

  // Disable button and show progress
  exportButton.disabled = true;
  exportButton.textContent = 'Exporting PNGs...';

  try {
    const timestamp = new Date().toISOString().replace(/[:.]/g, '-').slice(0, -5);

    // Capture PNG for each specified run
    for (let i = 0; i < runNumbers.length; i++) {
      const runIndex = runNumbers[i] - 1; // Convert to 0-based index
      
      // Update button to show progress
      exportButton.textContent = `Exporting ${i + 1}/${runNumbers.length}...`;
      
      // Switch to the specified run
      selectRun(runIndex);
      
      // Wait a bit for the render to complete
      await new Promise(resolve => setTimeout(resolve, 100));
      
      // Capture the container-inner with its background
      const canvas = await html2canvas(containerInner, {
        backgroundColor: null, // Use the element's own background
        scale: 2, // Capture at 2x resolution for better quality
        logging: false
      });
      
      // Convert canvas to blob
      await new Promise<void>((resolve) => {
        canvas.toBlob((blob) => {
          if (!blob) {
            console.error('Failed to create PNG for run ' + runNumbers[i]);
            resolve();
            return;
          }

          const filename = `run-${runNumbers[i]}-${timestamp}.png`;
          addDownloadLink(blob, filename, '🖼️');
          resolve();
        }, 'image/png');
      });
    }

    // Reset button
    exportButton.disabled = false;
    exportButton.textContent = 'Export PNGs';

  } catch (error) {
    console.error('Error exporting PNGs:', error);
    alert('Error exporting PNGs: ' + (error instanceof Error ? error.message : String(error)));
    
    exportButton.disabled = false;
    exportButton.textContent = 'Export PNGs';
  }
}

function setupScreenshot(): void {
  const screenshotButton = document.getElementById('screenshot-button') as HTMLButtonElement;

  if (!screenshotButton) return;

  screenshotButton.addEventListener('click', async () => {
    const containerInner = document.getElementById('container-inner');
    
    if (!containerInner) {
      alert('Container not found');
      return;
    }

    // Disable button during capture
    screenshotButton.disabled = true;
    screenshotButton.textContent = 'Capturing...';

    try {
      // Capture the container-inner with its background
      const canvas = await html2canvas(containerInner, {
        backgroundColor: null, // Use the element's own background
        scale: 2, // 2x resolution for crisp screenshots
        logging: false
      });

      // Convert canvas to blob
      canvas.toBlob((blob) => {
        if (!blob) {
          alert('Failed to create screenshot');
          screenshotButton.disabled = false;
          screenshotButton.textContent = 'Take Screenshot';
          return;
        }

        // Create filename with timestamp
        const timestamp = new Date().toISOString().replace(/[:.]/g, '-').slice(0, -5);
        const filename = `screenshot-run-${currentRunIndex + 1}-${timestamp}.png`;

        // Add download link
        addDownloadLink(blob, filename, '📸');

        // Reset button
        screenshotButton.disabled = false;
        screenshotButton.textContent = 'Take Screenshot';
      }, 'image/png');

    } catch (error) {
      console.error('Error creating screenshot:', error);
      alert('Error creating screenshot: ' + (error instanceof Error ? error.message : String(error)));
      
      screenshotButton.disabled = false;
      screenshotButton.textContent = 'Take Screenshot';
    }
  });
}

function addDownloadLink(blob: Blob, filename: string, icon: string = '💾'): void {
  const downloadLinksContainer = document.getElementById('download-links');
  
  if (!downloadLinksContainer) return;

  // Remove "no downloads" message if present
  const emptyState = downloadLinksContainer.querySelector('.download-links-empty');
  if (emptyState) {
    emptyState.remove();
  }

  // Create download link
  const url = URL.createObjectURL(blob);
  
  const linkElement = document.createElement('a');
  linkElement.className = 'download-link';
  linkElement.href = url;
  linkElement.download = filename;
  
  const iconSpan = document.createElement('span');
  iconSpan.className = 'download-link-icon';
  iconSpan.textContent = icon;
  
  const textSpan = document.createElement('span');
  textSpan.className = 'download-link-text';
  textSpan.textContent = filename;
  
  linkElement.appendChild(iconSpan);
  linkElement.appendChild(textSpan);
  
  // Add to the top of the list
  downloadLinksContainer.insertBefore(linkElement, downloadLinksContainer.firstChild);
  
  // Cleanup URL after download
  linkElement.addEventListener('click', () => {
    setTimeout(() => URL.revokeObjectURL(url), 100);
  });

  // Update download button states
  updateDownloadButtons();
}

function setupDownloadAll(): void {
  const downloadAllButton = document.getElementById('download-all-button') as HTMLButtonElement;
  
  if (!downloadAllButton) return;

  downloadAllButton.addEventListener('click', () => {
    const downloadLinksContainer = document.getElementById('download-links');
    if (!downloadLinksContainer) return;

    const links = downloadLinksContainer.querySelectorAll('.download-link') as NodeListOf<HTMLAnchorElement>;
    
    if (links.length === 0) return;

    // Download all files with a small delay between each to avoid browser blocking
    links.forEach((link, index) => {
      setTimeout(() => {
        link.click();
      }, index * 100); // 100ms delay between downloads
    });
  });
}

function setupClearDownloads(): void {
  const clearButton = document.getElementById('clear-downloads-button') as HTMLButtonElement;
  
  if (!clearButton) return;

  clearButton.addEventListener('click', () => {
    const downloadLinksContainer = document.getElementById('download-links');
    if (!downloadLinksContainer) return;

    // Clear all download links
    downloadLinksContainer.innerHTML = '';
    
    // Update button states
    updateDownloadButtons();
  });
}

function updateDownloadButtons(): void {
  const downloadAllButton = document.getElementById('download-all-button') as HTMLButtonElement;
  const clearButton = document.getElementById('clear-downloads-button') as HTMLButtonElement;
  const downloadLinksContainer = document.getElementById('download-links');
  
  if (!downloadAllButton || !clearButton || !downloadLinksContainer) return;

  const links = downloadLinksContainer.querySelectorAll('.download-link');
  const hasLinks = links.length > 0;
  
  downloadAllButton.disabled = !hasLinks;
  clearButton.disabled = !hasLinks;
}
