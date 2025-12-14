// ============================================
// SINGLE SOURCE OF TRUTH FOR ALL SETTINGS
// ============================================

export const CONFIG = {
  // Visibility
  sidebar: true,
  typePanel: false,
  solverRunPanel: false,
  codePanel: false,
  tracePanel: false,
  selectFirstTVar: false,
  showTVars: true,
  
  // Layout
  sidebarWidth: 250,
  levelHeight: 140,
  minNodeWidth: 90,
  maxNodeWidth: 850,
  minNodeSpacing: 50,
  subtreeSpacing: 50,
  minZoom: 0.25,
  maxZoom: 4,
  tvarPopOrderPauseInMs: 160,
  defaultZoomLevel: 1,
  defaultPanX: 200,
  defaultPanY: 0,
} as const;
