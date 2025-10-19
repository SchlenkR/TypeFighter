import type { JsNode, LinkData, TreeNode, Link, Position, EnvEntry } from './types';

export class TreeVisualizer {
  private readonly levelHeight = 140;
  private readonly minNodeWidth = 90;
  private readonly maxNodeWidth = 550;
  private readonly minNodeSpacing = 50;
  private readonly subtreeSpacing = 50;
  private readonly minLevelHeight = 40;
  private readonly maxLevelHeight = 320;
  private readonly envPanelWidth = 350;
  private readonly envPanelSpacing = 80;
  private readonly minZoom = 0.25;
  private readonly maxZoom = 4;

  private zoomLevel = 1;
  private panX = 360;
  private panY = 0;
  private isPanning = false;
  private activePointerId: number | null = null;
  private lastPanPosition = { x: 0, y: 0 };

  private nodeDataArray: JsNode[] = [];
  private linkDataArray: LinkData[] = [];
  private nodes = new Map<number, TreeNode>();
  private links: Link[] = [];
  private nodePositions = new Map<number, Position>();
  private nodeWidths = new Map<number, number>();
  private nodeHeights = new Map<number, number>();

  private container: HTMLElement;
  private solverRunPanel: HTMLElement;
  private envPanel: HTMLElement;
  private contentLayer: HTMLElement;
  private measurementContainer: HTMLElement;

  private envPanelExpressionNameEl!: HTMLElement;
  private envPanelExpressionCodeEl!: HTMLElement;
  private envPanelExpressionVarEl!: HTMLElement;
  private envPanelAdditionalEl!: HTMLElement;
  private envPanelTypeTextEl!: HTMLElement;
  private envPanelEnvContent!: HTMLElement;
  private envPanelSolverRunEl!: HTMLElement;

  private hoveredNodeKey: number | null = null;
  private selectedNodeKey: number | null = null;
  private didPan = false;
  private isFirstLoad = true;
  private allRuns: JsNode[] = [];
  private currentRunIndex: number = 0;

    constructor(allRuns: JsNode[], initialRunIndex: number = 0) {
    this.container = document.getElementById('tree-container')!;
    this.solverRunPanel = this.createSolverRunPanel();
    this.envPanel = this.createEnvPanel();
    this.contentLayer = this.createContentLayer();
    this.container.appendChild(this.solverRunPanel);
    this.container.appendChild(this.envPanel);
    this.container.appendChild(this.contentLayer);
    this.measurementContainer = this.createMeasurementContainer();

    this.handleEnvPanelMouseLeave = this.handleEnvPanelMouseLeave.bind(this);
    this.handlePointerDown = this.handlePointerDown.bind(this);
    this.handlePointerMove = this.handlePointerMove.bind(this);
    this.handlePointerUp = this.handlePointerUp.bind(this);
    this.handleContainerClick = this.handleContainerClick.bind(this);

    this.envPanel.addEventListener('mouseleave', this.handleEnvPanelMouseLeave);

    this.refreshEnvPanel();
    this.updateEnvPanelLayout();
    this.setupInteraction();
    this.applyTransform();

    this.allRuns = allRuns;
    if (allRuns.length > 0) {
      this.loadRun(allRuns[initialRunIndex] || allRuns[0], initialRunIndex);
    }
  }

  private createMeasurementContainer(): HTMLElement {
    const measurementHost = document.createElement('div');
    measurementHost.style.position = 'absolute';
    measurementHost.style.visibility = 'hidden';
    measurementHost.style.pointerEvents = 'none';
    measurementHost.style.left = '-10000px';
    measurementHost.style.top = '-10000px';
    measurementHost.style.width = 'auto';
    measurementHost.style.height = 'auto';
    document.body.appendChild(measurementHost);
    return measurementHost;
  }

  private createSolverRunPanel(): HTMLElement {
    const panel = document.createElement('div');
    panel.className = 'env-panel solver-run-panel';

    const body = document.createElement('div');
    body.className = 'env-panel-body';
    panel.appendChild(body);

    // Solver Run section
    const solverRunSection = document.createElement('div');
    solverRunSection.className = 'env-panel-section';
    body.appendChild(solverRunSection);

    const solverRunTitle = document.createElement('div');
    solverRunTitle.className = 'env-panel-section-title';
    solverRunTitle.textContent = 'SOLVER RUN:';
    solverRunSection.appendChild(solverRunTitle);

    const solverRunText = document.createElement('div');
    solverRunText.className = 'env-panel-solver-run';
    solverRunText.textContent = '-';
    solverRunSection.appendChild(solverRunText);

    this.envPanelSolverRunEl = solverRunText;

    return panel;
  }

  private createEnvPanel(): HTMLElement {
    const panel = document.createElement('div');
    panel.className = 'env-panel';

    const body = document.createElement('div');
    body.className = 'env-panel-body';
    panel.appendChild(body);

    const expressionSection = document.createElement('div');
    expressionSection.className = 'env-panel-section';
    body.appendChild(expressionSection);

    const expressionTitle = document.createElement('div');
    expressionTitle.className = 'env-panel-section-title';
    expressionTitle.textContent = 'EXPRESSION:';
    expressionSection.appendChild(expressionTitle);

    const expressionBox = document.createElement('div');
    expressionBox.className = 'env-panel-box env-panel-content';
    expressionSection.appendChild(expressionBox);

    const expressionHeader = document.createElement('div');
    expressionHeader.className = 'node-header env-panel-expression-header';
    expressionBox.appendChild(expressionHeader);

    const expressionTitleGroup = document.createElement('div');
    expressionTitleGroup.className = 'node-title-group env-panel-expression-title-group';
    expressionHeader.appendChild(expressionTitleGroup);

    const expressionName = document.createElement('div');
    expressionName.className = 'node-name env-panel-expression-name';
    expressionTitleGroup.appendChild(expressionName);

    const expressionCode = document.createElement('div');
    expressionCode.className = 'node-code env-panel-expression-code';
    expressionTitleGroup.appendChild(expressionCode);

    const expressionVar = document.createElement('div');
    expressionVar.className = 'tvar env-panel-expression-var';
    expressionHeader.appendChild(expressionVar);

    const expressionAdditional = document.createElement('div');
    expressionAdditional.className = 'node-code env-panel-additional env-panel-placeholder-text';
    expressionAdditional.textContent = '\u00a0';
    expressionBox.appendChild(expressionAdditional);

    const typeSection = document.createElement('div');
    typeSection.className = 'env-panel-section';
    body.appendChild(typeSection);

    const typeTitle = document.createElement('div');
    typeTitle.className = 'env-panel-section-title';
    typeTitle.textContent = 'TYPE';
    typeSection.appendChild(typeTitle);

    const typeBox = document.createElement('div');
    typeBox.className = 'env-panel-box env-panel-content';
    typeSection.appendChild(typeBox);

    const typeText = document.createElement('div');
    typeText.className = 'node-type env-panel-type-text';
    typeBox.appendChild(typeText);

    const envSection = document.createElement('div');
    envSection.className = 'env-panel-section';
    body.appendChild(envSection);

    const envSectionTitle = document.createElement('div');
    envSectionTitle.className = 'env-panel-section-title';
    envSectionTitle.textContent = 'ENVIRONMENT';
    envSection.appendChild(envSectionTitle);

    const envContent = document.createElement('div');
    envContent.className = 'env-panel-content';
    envSection.appendChild(envContent);

    this.envPanelExpressionNameEl = expressionName;
    this.envPanelExpressionCodeEl = expressionCode;
    this.envPanelExpressionVarEl = expressionVar;
    this.envPanelAdditionalEl = expressionAdditional;
    this.envPanelTypeTextEl = typeText;
    this.envPanelEnvContent = envContent;

    return panel;
  }

  private createContentLayer(): HTMLElement {
    const layer = document.createElement('div');
    layer.className = 'tree-content-layer';
    layer.style.paddingTop = '0px';
    layer.style.paddingRight = '0px';
    layer.style.paddingBottom = '0px';
    return layer;
  }

  private setEnvPanelText(text: string): void {
    this.envPanelEnvContent.textContent = text || '\u00a0';
    this.envPanelEnvContent.classList.toggle('env-panel-placeholder-text', !text || text === '\u00a0');
    this.envPanelEnvContent.scrollTop = 0;
  }

  private updateEnvPanelNodePreview(node: TreeNode | null): void {
    const nameEl = this.envPanelExpressionNameEl;
    const codeEl = this.envPanelExpressionCodeEl;
    const varEl = this.envPanelExpressionVarEl;
    const additionalEl = this.envPanelAdditionalEl;
    const typeEl = this.envPanelTypeTextEl;

    if (!node) {
      nameEl.textContent = '\u00a0';
      nameEl.classList.add('env-panel-placeholder-text');
      codeEl.textContent = '';
      codeEl.classList.add('env-panel-hidden');
      varEl.textContent = '';
      varEl.classList.add('env-panel-hidden');
      additionalEl.textContent = '\u00a0';
      additionalEl.classList.add('env-panel-placeholder-text');
      typeEl.textContent = '\u00a0';
      typeEl.classList.add('env-panel-placeholder-text');
      return;
    }

    nameEl.textContent = node.name;
    nameEl.classList.remove('env-panel-placeholder-text');

    if (node.code) {
      codeEl.textContent = node.code;
      codeEl.classList.remove('env-panel-hidden');
    } else {
      codeEl.textContent = '';
      codeEl.classList.add('env-panel-hidden');
    }

    varEl.textContent = 'tv_' + node.varNum;
    varEl.classList.remove('env-panel-hidden');

    if (node.additionalInfo) {
      additionalEl.textContent = node.additionalInfo;
      additionalEl.classList.remove('env-panel-placeholder-text');
    } else {
      additionalEl.textContent = '\u00a0';
      additionalEl.classList.add('env-panel-placeholder-text');
    }

    if (node.exprTyp) {
      typeEl.textContent = node.exprTyp;
      typeEl.classList.remove('env-panel-placeholder-text');
    } else {
      typeEl.textContent = '\u00a0';
      typeEl.classList.add('env-panel-placeholder-text');
    }
  }

  private renderEnvironmentEntries(env: EnvEntry[]): void {
    const content = this.envPanelEnvContent;
    if (env.length === 0) {
      content.classList.add('env-panel-placeholder-text');
      content.textContent = '\u00a0';
      return;
    }

    content.classList.remove('env-panel-placeholder-text');
    content.innerHTML = '';

    env.forEach(entry => {
      const row = document.createElement('div');
      row.className = 'env-panel-env-row';

      const identSpan = document.createElement('span');
      identSpan.className = 'env-panel-env-ident';
      identSpan.textContent = entry.ident;
      row.appendChild(identSpan);

      if (entry.varNum !== undefined) {
        const varSpan = document.createElement('span');
        varSpan.className = 'tvar';
        varSpan.textContent = `tv_${entry.varNum}`;
        row.appendChild(varSpan);
      }

      if (entry.solvedTyp) {
        const typeSpan = document.createElement('span');
        typeSpan.className = 'env-panel-env-type';
        typeSpan.textContent = entry.solvedTyp;
        row.appendChild(typeSpan);
      }

      content.appendChild(row);
    });
  }

  private updateEnvPanelLayout(): void {
    this.solverRunPanel.style.width = this.envPanelWidth + 'px';
    this.solverRunPanel.style.transformOrigin = 'top left';
    this.solverRunPanel.style.transform = `scale(${this.zoomLevel})`;
    
    this.envPanel.style.width = this.envPanelWidth + 'px';
    this.envPanel.style.transformOrigin = 'top left';
    this.envPanel.style.transform = `scale(${this.zoomLevel})`;
    this.contentLayer.style.paddingLeft = (this.envPanelWidth + this.envPanelSpacing) + 'px';
  }

  loadRun(jsNode: JsNode, runIndex?: number): void {
    // Update current run index if provided
    if (runIndex !== undefined) {
      this.currentRunIndex = runIndex;
      this.updateSolverRunDisplay();
    }

    // Convert hierarchical tree to flat nodes and links
    const { nodes, links } = this.flattenHierarchicalTree(jsNode);
    
    if (this.isFirstLoad) {
      // First load: do full layout calculation
      this.nodeDataArray = nodes;
      this.linkDataArray = links;
      
      this.parseData();
      this.calculateNodeDimensions();
      this.calculateLayout();
      this.updateEnvPanelLayout();
      this.render();
      
      this.isFirstLoad = false;
    } else {
      // Subsequent loads: just update node data without recalculating layout
      this.updateNodeData(nodes);
      this.updateNodeElements();
      this.refreshEnvPanel();
    }
  }

  private updateSolverRunDisplay(): void {
    this.envPanelSolverRunEl.textContent = `${this.currentRunIndex + 1}`;
  }

  private updateNodeData(nodes: JsNode[]): void {
    // Update the existing TreeNode objects with new data
    nodes.forEach(nodeData => {
      const existingNode = this.nodes.get(nodeData.key);
      if (existingNode) {
        existingNode.name = nodeData.name;
        existingNode.code = nodeData.code;
        existingNode.varNum = nodeData.varNum;
        existingNode.additionalInfo = nodeData.additionalInfo;
        existingNode.exprTyp = nodeData.exprTyp;
        existingNode.env = nodeData.env || [];
      }
    });
  }

  private updateNodeElements(): void {
    // Update the DOM elements for each node without recalculating positions
    this.nodes.forEach(node => {
      const nodeElement = this.contentLayer.querySelector<HTMLElement>(
        `.node[data-key="${node.key}"]`
      );
      if (nodeElement) {
        // Update the node header
        const nameEl = nodeElement.querySelector('.node-name');
        if (nameEl) nameEl.textContent = node.name;
        
        const codeEl = nodeElement.querySelector('.node-code');
        if (codeEl) {
          if (node.code) {
            codeEl.textContent = node.code;
            codeEl.classList.remove('env-panel-hidden');
          } else {
            codeEl.textContent = '';
            codeEl.classList.add('env-panel-hidden');
          }
        }
        
        const varEl = nodeElement.querySelector('.tvar');
        if (varEl) varEl.textContent = 'tv_' + node.varNum;
        
        // Update the type
        const typeEl = nodeElement.querySelector('.node-type');
        if (typeEl) {
          typeEl.textContent = node.exprTyp;
        } else if (node.exprTyp) {
          // Type element doesn't exist but we have a type, create it
          const newTypeEl = document.createElement('div');
          newTypeEl.className = 'node-type';
          newTypeEl.textContent = node.exprTyp;
          nodeElement.appendChild(newTypeEl);
        }
      }
    });
  }

  private flattenHierarchicalTree(root: JsNode): { nodes: JsNode[], links: LinkData[] } {
    const nodes: JsNode[] = [];
    const links: LinkData[] = [];
    const visited = new Set<number>();

    const traverse = (node: JsNode) => {
      if (visited.has(node.key)) return;
      visited.add(node.key);

      // Add the node itself
      nodes.push({
        key: node.key,
        name: node.name,
        code: node.code,
        varNum: node.varNum,
        additionalInfo: node.additionalInfo,
        exprTyp: node.exprTyp,
        env: node.env || [],
        children: [] // Will be rebuilt by parseData
      });

      // Process children
      if (node.children && Array.isArray(node.children)) {
        for (const child of node.children) {
          links.push({ from: node.key, to: child.key });
          traverse(child);
        }
      }
    };

    traverse(root);
    return { nodes, links };
  }

  private parseData(): void {
    this.nodeDataArray.forEach(nodeData => {
      this.nodes.set(nodeData.key, {
        ...nodeData,
        children: [],
        parents: [],
      });
    });

    this.linkDataArray.forEach(link => {
      const parent = this.nodes.get(link.from)!;
      const child = this.nodes.get(link.to)!;
      parent.children.push(child);
      child.parents.push(parent);
      this.links.push({ parent, child });
    });
  }

  private calculateNodeDimensions(): void {
    this.measurementContainer.innerHTML = '';

    // Use the last solver run for measurements if available
    const lastRun = this.allRuns.length > 0 ? this.allRuns[this.allRuns.length - 1] : null;
    let lastRunNodes: Map<number, JsNode> | null = null;
    
    if (lastRun) {
      const { nodes } = this.flattenHierarchicalTree(lastRun);
      lastRunNodes = new Map(nodes.map(n => [n.key, n]));
    }

    this.nodes.forEach(node => {
      // Create a temporary node with data from the last run for measurement
      const measurementNode = lastRunNodes?.get(node.key) 
        ? { ...node, ...lastRunNodes.get(node.key) } 
        : node;
      
      const measurementElement = this.buildNodeElement(measurementNode as TreeNode);
      measurementElement.style.position = 'static';
      measurementElement.style.left = 'auto';
      measurementElement.style.top = 'auto';
      measurementElement.style.width = 'auto';

      this.measurementContainer.appendChild(measurementElement);

      const naturalWidth = measurementElement.offsetWidth;
      const clampedWidth = Math.max(
        this.minNodeWidth,
        Math.min(this.maxNodeWidth, naturalWidth)
      );

      if (clampedWidth !== naturalWidth) {
        measurementElement.style.width = clampedWidth + 'px';
      }

      const finalHeight = measurementElement.offsetHeight;

      this.nodeWidths.set(node.key, clampedWidth);
      this.nodeHeights.set(node.key, finalHeight);

      this.measurementContainer.removeChild(measurementElement);
    });
  }

  private calculateLayout(): void {
    const roots = Array.from(this.nodes.values()).filter(
      node => node.parents.length === 0
    );

    const subtreeWidths = new Map<number, number>();
    roots.forEach(root => {
      this.calculateSubtreeWidth(root, subtreeWidths, new Set());
    });

    let currentX = 50;
    roots.forEach(root => {
      currentX = this.positionSubtree(
        root,
        0,
        currentX,
        subtreeWidths,
        new Set()
      );
      currentX += 50;
    });
  }

  private calculateSubtreeWidth(
    node: TreeNode,
    subtreeWidths: Map<number, number>,
    visited: Set<number>
  ): number {
    if (visited.has(node.key)) {
      return subtreeWidths.get(node.key) || 0;
    }
    visited.add(node.key);

    const nodeWidth = this.nodeWidths.get(node.key)!;

    if (node.children.length === 0) {
      subtreeWidths.set(node.key, nodeWidth);
      return nodeWidth;
    } else {
      let childrenTotalWidth = 0;
      node.children.forEach((child, index) => {
        const childSubtreeWidth = this.calculateSubtreeWidth(
          child,
          subtreeWidths,
          visited
        );
        childrenTotalWidth += childSubtreeWidth;
        if (index < node.children.length - 1) {
          const spacing = Math.max(
            this.subtreeSpacing,
            this.minNodeSpacing
          );
          childrenTotalWidth += spacing;
        }
      });

      const subtreeWidth = Math.max(nodeWidth, childrenTotalWidth);
      subtreeWidths.set(node.key, subtreeWidth);
      return subtreeWidth;
    }
  }

  private positionSubtree(
    node: TreeNode,
    level: number,
    startX: number,
    subtreeWidths: Map<number, number>,
    visited: Set<number>
  ): number {
    if (visited.has(node.key)) {
      return startX;
    }
    visited.add(node.key);

    const y = level * this.levelHeight + 50;
    const nodeWidth = this.nodeWidths.get(node.key)!;
    const subtreeWidth = subtreeWidths.get(node.key)!;

    if (node.children.length === 0) {
      this.nodePositions.set(node.key, { x: startX, y });
      return startX + subtreeWidth;
    } else {
      let currentChildX = startX;

      node.children.forEach((child, index) => {
        const childStartX = currentChildX;
        currentChildX = this.positionSubtree(
          child,
          level + 1,
          childStartX,
          subtreeWidths,
          visited
        );

        if (index < node.children.length - 1) {
          currentChildX += this.subtreeSpacing;
        }
      });

      this.enforceMinimumSpacing(node.children, level + 1);

      const updatedChildPositions = node.children.map(child =>
        this.nodePositions.get(child.key)!
      );
      
      if (updatedChildPositions.length > 0) {
        const leftmostChild = Math.min(
          ...updatedChildPositions.map(pos => pos.x)
        );
        const rightmostChild = Math.max(
          ...updatedChildPositions.map(
            (pos, index) =>
              pos.x + this.nodeWidths.get(node.children[index].key)!
          )
        );
        const childrenCenter = (leftmostChild + rightmostChild) / 2;
        const parentX = childrenCenter - nodeWidth / 2;

        const minParentX = startX;
        const maxParentX = startX + subtreeWidth - nodeWidth;
        const finalParentX = Math.max(
          minParentX,
          Math.min(maxParentX, parentX)
        );

        this.nodePositions.set(node.key, { x: finalParentX, y });

        if (node.children.length === 1) {
          const child = node.children[0];
          if (child.parents.length === 1) {
            const childPos = this.nodePositions.get(child.key)!;
            const childWidth = this.nodeWidths.get(child.key)!;
            const parentCenterX = finalParentX + nodeWidth / 2;
            const childCenterX = childPos.x + childWidth / 2;
            const delta = parentCenterX - childCenterX;
            if (Math.abs(delta) > 0.5) {
              this.shiftSubtree(child, delta);
            }
          }
        }
      } else {
        this.nodePositions.set(node.key, { x: startX, y });
      }

      return startX + subtreeWidth;
    }
  }

  private enforceMinimumSpacing(children: TreeNode[], level: number): void {
    if (children.length <= 1) return;

    const sortedChildren = [...children].sort((a, b) => {
      const posA = this.nodePositions.get(a.key)!;
      const posB = this.nodePositions.get(b.key)!;
      return posA.x - posB.x;
    });

    for (let i = 1; i < sortedChildren.length; i++) {
      const prevChild = sortedChildren[i - 1];
      const currentChild = sortedChildren[i];

      const prevPos = this.nodePositions.get(prevChild.key)!;
      const currentPos = this.nodePositions.get(currentChild.key)!;
      const prevWidth = this.nodeWidths.get(prevChild.key)!;

      const minX = prevPos.x + prevWidth + this.minNodeSpacing;

      if (currentPos.x < minX) {
        const shift = minX - currentPos.x;
        for (let j = i; j < sortedChildren.length; j++) {
          const nodeToShift = sortedChildren[j];
          const posToShift = this.nodePositions.get(nodeToShift.key)!;
          this.nodePositions.set(nodeToShift.key, {
            x: posToShift.x + shift,
            y: posToShift.y,
          });
        }
      }
    }
  }

  private shiftSubtree(node: TreeNode, deltaX: number): void {
    if (deltaX === 0) return;

    const visited = new Set<number>();
    const stack = [node];

    while (stack.length > 0) {
      const current = stack.pop()!;
      if (visited.has(current.key)) continue;
      visited.add(current.key);

      const pos = this.nodePositions.get(current.key)!;
      this.nodePositions.set(current.key, {
        x: pos.x + deltaX,
        y: pos.y,
      });

      if (current.children.length === 0) continue;

      current.children.forEach(childNode => {
        if (childNode.parents.length === 1) {
          stack.push(childNode);
        }
      });
    }
  }

  private render(): void {
    this.contentLayer.innerHTML = '';
    this.hoveredNodeKey = null;

    this.nodes.forEach(node => {
      this.renderNode(node);
    });

    this.updateActualNodeHeights();
    this.renderConnections();
    this.updateSelectionHighlight();
    this.refreshEnvPanel();
    this.applyTransform();
  }

  private updateActualNodeHeights(): void {
    this.nodes.forEach(node => {
      const nodeElement = this.contentLayer.querySelector<HTMLElement>(
        `.node[data-key="${node.key}"]`
      );
      if (nodeElement) {
        const actualHeight = nodeElement.offsetHeight;
        this.nodeHeights.set(node.key, actualHeight);
      }
    });
  }

  private renderConnections(): void {
    const parentToChildren = new Map<number, TreeNode[]>();

    this.links.forEach(link => {
      if (!parentToChildren.has(link.parent.key)) {
        parentToChildren.set(link.parent.key, []);
      }
      parentToChildren.get(link.parent.key)!.push(link.child);
    });

    parentToChildren.forEach((children, parentKey) => {
      const parentElement = this.contentLayer.querySelector<HTMLElement>(
        `.node[data-key="${parentKey}"]`
      );
      if (!parentElement) return;

      const parentCenterX = parentElement.offsetLeft + parentElement.offsetWidth / 2;
      const parentBottomY = parentElement.offsetTop + parentElement.offsetHeight;

      const childDetails = children
        .map(child => {
          const childElement = this.contentLayer.querySelector<HTMLElement>(
            `.node[data-key="${child.key}"]`
          );
          if (!childElement) return null;
          return {
            centerX: childElement.offsetLeft + childElement.offsetWidth / 2,
            topY: childElement.offsetTop,
          };
        })
        .filter((detail): detail is { centerX: number; topY: number } => detail !== null);

      if (childDetails.length === 0) return;

      const minChildTop = Math.min(...childDetails.map(detail => detail.topY));
      const gapBetweenLevels = Math.max(0, minChildTop - parentBottomY);
      const verticalDropDistance = gapBetweenLevels / 2;
      const horizontalLineY = parentBottomY + verticalDropDistance;

      const parentVerticalLine = document.createElement('div');
      parentVerticalLine.className = 'connection-line';
      parentVerticalLine.style.left = parentCenterX + 'px';
      parentVerticalLine.style.top = parentBottomY + 'px';
      parentVerticalLine.style.width = '2px';
      parentVerticalLine.style.height = verticalDropDistance + 'px';
      this.contentLayer.appendChild(parentVerticalLine);

      const minChildX = Math.min(...childDetails.map(detail => detail.centerX));
      const maxChildX = Math.max(...childDetails.map(detail => detail.centerX));

      const horizontalLine = document.createElement('div');
      horizontalLine.className = 'connection-line';
      horizontalLine.style.left = minChildX + 'px';
      horizontalLine.style.top = horizontalLineY + 'px';
      horizontalLine.style.width = maxChildX - minChildX + 'px';
      horizontalLine.style.height = '2px';
      this.contentLayer.appendChild(horizontalLine);

      childDetails.forEach(detail => {
        const childVerticalLine = document.createElement('div');
        childVerticalLine.className = 'connection-line';
        childVerticalLine.style.left = detail.centerX + 'px';
        childVerticalLine.style.top = horizontalLineY + 'px';
        childVerticalLine.style.width = '2px';
        childVerticalLine.style.height = detail.topY - horizontalLineY + 'px';
        this.contentLayer.appendChild(childVerticalLine);

        const arrow = document.createElement('div');
        arrow.className = 'connection-arrow';
        arrow.style.left = detail.centerX - 6 + 'px';
        arrow.style.top = detail.topY - 8 + 'px';
        this.contentLayer.appendChild(arrow);
      });
    });
  }

  private setupInteraction(): void {
    this.container.addEventListener('pointerdown', this.handlePointerDown);
    window.addEventListener('pointermove', this.handlePointerMove);
    window.addEventListener('pointerup', this.handlePointerUp);
    window.addEventListener('pointercancel', this.handlePointerUp);
    this.container.addEventListener('click', this.handleContainerClick);

    // Handle Escape key to deselect nodes
    window.addEventListener('keydown', (event) => {
      if (event.key === 'Escape' && this.selectedNodeKey !== null) {
        this.selectedNodeKey = null;
        this.updateSelectionHighlight();
        this.refreshEnvPanel();
      }
    });

    this.container.addEventListener('wheel', (event) => {
      if (this.envPanel.contains(event.target as Node)) return;
      if (event.deltaY === 0) return;

      const zoomFactor = event.deltaY > 0 ? 0.9 : 1.1;
      this.zoom(zoomFactor, { x: event.clientX, y: event.clientY });
      event.preventDefault();
    }, { passive: false });
  }

  private handlePointerDown(event: PointerEvent): void {
    if (event.button !== 0) return;

    this.didPan = false;

    if (this.envPanel.contains(event.target as Node) || (event.target as HTMLElement).closest('.node')) {
      return;
    }

    this.isPanning = true;
    this.activePointerId = event.pointerId;
    this.lastPanPosition = { x: event.clientX, y: event.clientY };
    this.container.classList.add('panning');
    this.container.setPointerCapture?.(event.pointerId);
    event.preventDefault();
  }

  private handlePointerMove(event: PointerEvent): void {
    if (!this.isPanning || event.pointerId !== this.activePointerId) return;

    const deltaX = event.clientX - this.lastPanPosition.x;
    const deltaY = event.clientY - this.lastPanPosition.y;
    this.lastPanPosition = { x: event.clientX, y: event.clientY };

    this.panX += deltaX;
    this.panY += deltaY;
    if (deltaX !== 0 || deltaY !== 0) {
      this.didPan = true;
    }
    this.applyTransform();
    event.preventDefault();
  }

  private handlePointerUp(event: PointerEvent): void {
    if (event.pointerId !== this.activePointerId) return;

    this.isPanning = false;
    this.activePointerId = null;
    this.container.classList.remove('panning');
    this.container.releasePointerCapture?.(event.pointerId);
  }

  private handleContainerClick(event: MouseEvent): void {
    if (this.didPan) {
      this.didPan = false;
      return;
    }

    if (this.envPanel.contains(event.target as Node) || (event.target as HTMLElement).closest('.node')) {
      return;
    }

    if (this.selectedNodeKey !== null) {
      this.selectedNodeKey = null;
      this.updateSelectionHighlight();
      this.refreshEnvPanel();
    }
  }

  private handleEnvPanelMouseLeave(event: MouseEvent): void {
    const nextTarget = event.relatedTarget as HTMLElement | null;
    if (nextTarget?.closest('.node') || this.envPanel.contains(nextTarget)) {
      return;
    }

    if (this.hoveredNodeKey !== null) {
      this.hoveredNodeKey = null;
      this.refreshEnvPanel();
    }
  }

  private applyTransform(): void {
    this.contentLayer.style.transform = `translate(${this.panX}px, ${this.panY}px) scale(${this.zoomLevel})`;
    this.contentLayer.style.transformOrigin = 'top left';
    this.updateEnvPanelLayout();
  }

  private buildNodeElement(node: TreeNode): HTMLElement {
    const nodeElement = document.createElement('div');
    nodeElement.className = 'node';
    nodeElement.dataset.key = String(node.key);

    const header = document.createElement('div');
    header.className = 'node-header';

    const titleGroup = document.createElement('div');
    titleGroup.className = 'node-title-group';

    const nameEl = document.createElement('div');
    nameEl.className = 'node-name';
    nameEl.textContent = node.name;
    titleGroup.appendChild(nameEl);

    if (node.code) {
      const codeEl = document.createElement('div');
      codeEl.className = 'node-code';
      codeEl.textContent = node.code;
      titleGroup.appendChild(codeEl);
    }

    const varEl = document.createElement('div');
    varEl.className = 'tvar';
    varEl.textContent = 'tv_' + node.varNum;

    header.appendChild(titleGroup);
    header.appendChild(varEl);
    nodeElement.appendChild(header);

    if (node.exprTyp) {
      const typeEl = document.createElement('div');
      typeEl.className = 'node-type';
      typeEl.textContent = node.exprTyp;
      nodeElement.appendChild(typeEl);
    }

    return nodeElement;
  }

  private renderNode(node: TreeNode): void {
    const position = this.nodePositions.get(node.key);
    if (!position) return;

    const nodeWidth = this.nodeWidths.get(node.key)!;
    const nodeElement = this.buildNodeElement(node);
    nodeElement.style.left = position.x + 'px';
    nodeElement.style.top = position.y + 'px';
    nodeElement.style.width = nodeWidth + 'px';
    this.attachNodeEvents(node, nodeElement);
    this.contentLayer.appendChild(nodeElement);
  }

  private attachNodeEvents(node: TreeNode, nodeElement: HTMLElement): void {
    nodeElement.addEventListener('mouseenter', () => {
      this.hoveredNodeKey = node.key;
      this.refreshEnvPanel();
    });

    nodeElement.addEventListener('mouseleave', (event) => {
      const nextTarget = event.relatedTarget as HTMLElement | null;
      if (nextTarget?.closest('.node') || this.envPanel.contains(nextTarget)) {
        return;
      }
      this.hoveredNodeKey = null;
      this.refreshEnvPanel();
    });

    nodeElement.addEventListener('click', (event) => {
      event.stopPropagation();
      this.selectedNodeKey = node.key;
      this.updateSelectionHighlight();
      this.refreshEnvPanel();
    });
  }

  private updateSelectionHighlight(): void {
    const nodeElements = this.contentLayer.querySelectorAll<HTMLElement>('.node');
    const selectedKeyString = this.selectedNodeKey !== null ? String(this.selectedNodeKey) : null;

    nodeElements.forEach(nodeElement => {
      const isSelected = selectedKeyString !== null && nodeElement.dataset.key === selectedKeyString;
      nodeElement.classList.toggle('selected', isSelected);
    });
  }

  private refreshEnvPanel(): void {
    const activeKey = this.hoveredNodeKey !== null ? this.hoveredNodeKey : this.selectedNodeKey;
    const node = activeKey !== null ? this.nodes.get(activeKey) ?? null : null;

    this.updateEnvPanelNodePreview(node);
    this.renderEnvironmentEntries(node ? node.env : []);
  }

  zoom(factor: number, focalPoint: { x: number; y: number } | null = null): void {
    const oldZoom = this.zoomLevel;
    const newZoom = Math.min(this.maxZoom, Math.max(this.minZoom, oldZoom * factor));
    const zoomChange = newZoom / oldZoom;

    if (focalPoint && zoomChange !== 1) {
      const layerRect = this.contentLayer.getBoundingClientRect();
      const offsetX = focalPoint.x - layerRect.left;
      const offsetY = focalPoint.y - layerRect.top;

      this.panX = offsetX - (offsetX - this.panX) * zoomChange;
      this.panY = offsetY - (offsetY - this.panY) * zoomChange;
    }

    this.zoomLevel = newZoom;
    this.applyTransform();
  }

  resetZoom(): void {
    this.zoomLevel = 1;
    this.panX = 0;
    this.panY = 0;
    this.applyTransform();
  }
}
