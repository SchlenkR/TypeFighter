export interface EnvEntry {
  ident: string;
  varNum?: number;
  solvedTyp?: string;
}

// JsNode from F# - hierarchical structure with children
export interface JsNode {
  key: number;
  name: string;
  code: string;
  varNum: number;
  additionalInfo: string;
  exprTyp: string;
  env: EnvEntry[];
  children: JsNode[];
}

export interface LinkData {
  from: number;
  to: number;
}

export interface TreeNode {
  key: number;
  name: string;
  code: string;
  varNum: string | number;
  additionalInfo: string;
  exprTyp: string;
  env: EnvEntry[];
  children: TreeNode[];
  parents: TreeNode[];
}

export interface Link {
  parent: TreeNode;
  child: TreeNode;
}

export interface Position {
  x: number;
  y: number;
}

declare global {
  interface Window {
    solverRuns: JsNode[];
  }
}
