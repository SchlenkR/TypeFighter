import { TreeVisualizer } from './TreeVisualizer';
import type { JsNode } from './types';

let treeViz: TreeVisualizer;
let runButtons: HTMLButtonElement[] = [];

function getRunLabel(jsNode: JsNode, index: number): string {
  // Just use the number (1-indexed)
  return `${index + 1}`;
}

function selectRun(index: number): void {
  if (!treeViz) return;

  const runs = window.solverRuns;
  const nextRun = runs[index];
  if (!nextRun) return;

  runButtons.forEach((button, buttonIndex) => {
    button.classList.toggle('active', buttonIndex === index);
  });

  treeViz.loadRun(nextRun);
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
  const runs = window.solverRuns || [];
  treeViz = new TreeVisualizer(runs, 0);
  setupRunButtons(runs);
});
