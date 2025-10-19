export interface EnvEntry {
  ident: string;
  varNum?: number;
  solvedTyp?: string;
}

export interface NodeData {
  key: number;
  name: string;
  code: string;
  varNum: string | number;
  additionalInfo: string;
  exprTyp: string;
  env: EnvEntry[];
}

export interface LinkData {
  from: number;
  to: number;
}

export interface Run {
  jsNodes: NodeData[];
  jsLinks: LinkData[];
  label?: string;
  name?: string;
  title?: string;
  id?: string | number;
}

export interface TreeNode extends NodeData {
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
    solverRuns: Run[];
  }
}
