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
}

function nextRun(): void {
  const treesForSolverRuns = window.treesForSolverRuns;
  if (currentRunIndex < treesForSolverRuns.length - 1) {
    selectRun(currentRunIndex + 1);
  }
}

function previousRun(): void {
  if (currentRunIndex > 0) {
    selectRun(currentRunIndex - 1);
  }
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

  const inputPanelToggle = document.getElementById('input-panel-toggle') as HTMLInputElement;
  
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

  const selectFirstTVarCheckbox = document.getElementById('select-first-tvar') as HTMLInputElement;
  if (selectFirstTVarCheckbox) {
    selectFirstTVarCheckbox.addEventListener('change', () => {
      if (treeViz) {
        treeViz.toggleSelectFirstTVar(selectFirstTVarCheckbox.checked);
      }
    });
  }

  // Wire up control panel textarea to update input panel
  const controlInputTextarea = document.getElementById('control-input-text') as HTMLTextAreaElement;
  if (controlInputTextarea) {
    controlInputTextarea.addEventListener('input', () => {
      if (treeViz) {
        // Always mirror text into CODE panel
        treeViz.updateInputText(controlInputTextarea.value);

        // Auto-open CODE panel if it's hidden so the user immediately sees the text reflected
        const inputToggle = document.getElementById('input-panel-toggle') as HTMLInputElement | null;
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
  const treeContainer = document.getElementById('tree-container');
  
  if (!treeContainer) {
    alert('Tree container not found');
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
      
      // Get the background color from the body element
      const bodyBgColor = window.getComputedStyle(document.body).backgroundColor;
      
      // Capture the tree container as an image with higher resolution
      const canvas = await html2canvas(treeContainer, {
        backgroundColor: bodyBgColor,
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
      // Create download link
      const url = URL.createObjectURL(blob);
      const link = document.createElement('a');
      link.download = 'solver-runs-animation.gif';
      link.href = url;
      link.click();
      
      // Cleanup
      URL.revokeObjectURL(url);
      
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
