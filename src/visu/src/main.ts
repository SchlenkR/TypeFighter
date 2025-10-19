import { TreeVisualizer } from './TreeVisualizer';
import type { Run } from './types';

let treeViz: TreeVisualizer;
let runButtons: HTMLButtonElement[] = [];

function getRunLabel(run: Run, index: number): string {
  return run.label || run.name || run.title || (run.id ? `Instance ${run.id}` : `Instance ${index + 1}`);
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

function setupRunButtons(runs: Run[]): void {
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
  const initialRun = runs[0] || null;
  treeViz = new TreeVisualizer(initialRun);
  setupRunButtons(runs);
});
