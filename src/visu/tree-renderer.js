/**
 * Hand-rolled tree visualization for TypeFighter
 * Replaces go.js with custom SVG-based rendering
 */

class TreeRenderer {
    constructor(containerId) {
        this.container = document.getElementById(containerId);
        this.svg = null;
        this.nodes = new Map();
        this.links = [];
        this.nodePositions = new Map();
        this.config = {
            nodeWidth: 200,
            nodeHeight: 80,
            levelHeight: 140,
            horizontalSpacing: 50,
            showEnvironment: false,
            zoom: 1
        };
        this.init();
    }

    init() {
        this.createSVG();
        this.parseData();
        this.calculateLayout();
        this.render();
    }

    createSVG() {
        this.svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
        this.svg.style.width = '100%';
        this.svg.style.height = '100%';
        this.svg.style.minWidth = '1200px';
        this.svg.style.minHeight = '800px';
        this.container.appendChild(this.svg);

        // Create groups for different layers
        this.connectionsGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');
        this.connectionsGroup.setAttribute('class', 'connections');
        this.svg.appendChild(this.connectionsGroup);

        this.nodesGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');
        this.nodesGroup.setAttribute('class', 'nodes');
        this.svg.appendChild(this.nodesGroup);
    }

    parseData() {
        // Create nodes map
        window.nodeDataArray.forEach(nodeData => {
            this.nodes.set(nodeData.key, {
                ...nodeData,
                children: [],
                parents: []
            });
        });

        // Create links and establish parent-child relationships
        window.linkDataArray.forEach(link => {
            const parent = this.nodes.get(link.from);
            const child = this.nodes.get(link.to);
            
            if (parent && child) {
                parent.children.push(child);
                child.parents.push(parent);
                this.links.push({ parent, child });
            }
        });
    }

    calculateLayout() {
        // Find root nodes (nodes with no parents)
        const roots = Array.from(this.nodes.values()).filter(node => node.parents.length === 0);
        
        // Calculate positions using a top-down approach
        let startX = 50;
        roots.forEach(root => {
            startX = this.calculateSubtreeLayout(root, 0, startX, new Set());
            startX += this.config.horizontalSpacing;
        });
    }

    calculateSubtreeLayout(node, level, startX, visited) {
        if (visited.has(node.key)) {
            return startX;
        }
        visited.add(node.key);

        const y = level * this.config.levelHeight + 50;
        
        if (node.children.length === 0) {
            // Leaf node
            this.nodePositions.set(node.key, { x: startX, y });
            return startX + this.config.nodeWidth + this.config.horizontalSpacing;
        } else {
            // Internal node - position children first
            let currentX = startX;
            const childPositions = [];
            
            node.children.forEach(child => {
                currentX = this.calculateSubtreeLayout(child, level + 1, currentX, visited);
                childPositions.push(this.nodePositions.get(child.key));
            });
            
            // Center parent over children
            if (childPositions.length > 0) {
                const leftmost = Math.min(...childPositions.map(pos => pos.x));
                const rightmost = Math.max(...childPositions.map(pos => pos.x + this.config.nodeWidth));
                const centerX = (leftmost + rightmost) / 2 - this.config.nodeWidth / 2;
                this.nodePositions.set(node.key, { x: centerX, y });
            }
            
            return currentX;
        }
    }

    render() {
        // Clear previous content
        this.connectionsGroup.innerHTML = '';
        this.nodesGroup.innerHTML = '';
        
        // Render connections
        this.renderConnections();
        
        // Render nodes
        this.nodes.forEach(node => {
            this.renderNode(node);
        });
    }

    renderConnections() {
        this.links.forEach(link => {
            const parentPos = this.nodePositions.get(link.parent.key);
            const childPos = this.nodePositions.get(link.child.key);
            
            if (parentPos && childPos) {
                this.drawConnection(parentPos, childPos);
            }
        });
    }

    drawConnection(parentPos, childPos) {
        const startX = parentPos.x + this.config.nodeWidth / 2;
        const startY = parentPos.y + this.config.nodeHeight;
        const endX = childPos.x + this.config.nodeWidth / 2;
        const endY = childPos.y;
        
        const midY = startY + (endY - startY) / 2;

        // Create path for connection
        const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
        const pathData = `M ${startX} ${startY} L ${startX} ${midY} L ${endX} ${midY} L ${endX} ${endY}`;
        path.setAttribute('d', pathData);
        path.setAttribute('stroke', '#333');
        path.setAttribute('stroke-width', '2');
        path.setAttribute('fill', 'none');
        this.connectionsGroup.appendChild(path);

        // Arrow head
        const arrowHead = document.createElementNS('http://www.w3.org/2000/svg', 'polygon');
        const arrowSize = 6;
        const arrowPoints = `${endX},${endY} ${endX-arrowSize},${endY-arrowSize} ${endX+arrowSize},${endY-arrowSize}`;
        arrowHead.setAttribute('points', arrowPoints);
        arrowHead.setAttribute('fill', '#333');
        this.connectionsGroup.appendChild(arrowHead);
    }

    renderNode(node) {
        const position = this.nodePositions.get(node.key);
        if (!position) return;

        // Node group
        const nodeGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');
        nodeGroup.setAttribute('class', 'node');
        nodeGroup.setAttribute('transform', `translate(${position.x}, ${position.y})`);

        // Node rectangle
        const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
        rect.setAttribute('width', this.config.nodeWidth);
        rect.setAttribute('height', this.calculateNodeHeight(node));
        rect.setAttribute('fill', 'white');
        rect.setAttribute('stroke', 'darkblue');
        rect.setAttribute('stroke-width', '2');
        rect.setAttribute('rx', '4');
        nodeGroup.appendChild(rect);

        // Text content
        let yOffset = 20;
        
        // Name and variable number
        const nameText = this.createText(node.name, 10, yOffset, 'bold 12px sans-serif');
        nodeGroup.appendChild(nameText);
        
        const varText = this.createText(`tv_${node.varNum}`, this.config.nodeWidth - 10, yOffset, '10px sans-serif', 'end');
        varText.setAttribute('fill', '#666');
        nodeGroup.appendChild(varText);
        
        yOffset += 20;

        // Additional info
        if (node.additionalInfo && node.additionalInfo.trim()) {
            const additionalText = this.createMultilineText(node.additionalInfo, 10, yOffset, '10px Consolas', '#666');
            nodeGroup.appendChild(additionalText);
            yOffset += 15;
        }

        // Expression type
        if (node.exprTyp && node.exprTyp.trim()) {
            yOffset += 5;
            const typeText = this.createText(node.exprTyp, 10, yOffset, 'bold 14px Consolas', 'start', 'green');
            nodeGroup.appendChild(typeText);
            yOffset += 20;
        }

        // Environment (if enabled)
        if (this.config.showEnvironment && node.env && node.env.trim()) {
            yOffset += 5;
            const envText = this.createMultilineText(node.env, 10, yOffset, '9px Consolas', '#555');
            nodeGroup.appendChild(envText);
        }

        this.nodesGroup.appendChild(nodeGroup);
    }

    createText(text, x, y, font = '12px sans-serif', anchor = 'start', fill = 'black') {
        const textElement = document.createElementNS('http://www.w3.org/2000/svg', 'text');
        textElement.setAttribute('x', x);
        textElement.setAttribute('y', y);
        textElement.setAttribute('font', font);
        textElement.setAttribute('text-anchor', anchor);
        textElement.setAttribute('fill', fill);
        textElement.textContent = text;
        return textElement;
    }

    createMultilineText(text, x, y, font = '12px sans-serif', fill = 'black') {
        const lines = text.split('\n');
        const textGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');
        
        lines.forEach((line, index) => {
            const textElement = this.createText(line, x, y + (index * 12), font, 'start', fill);
            textGroup.appendChild(textElement);
        });
        
        return textGroup;
    }

    calculateNodeHeight(node) {
        let height = 40; // Base height for name and type
        
        if (node.additionalInfo && node.additionalInfo.trim()) {
            height += 20;
        }
        
        if (node.exprTyp && node.exprTyp.trim()) {
            height += 25;
        }
        
        if (this.config.showEnvironment && node.env && node.env.trim()) {
            const lines = node.env.split('\n').length;
            height += lines * 12 + 10;
        }
        
        return Math.max(height, this.config.nodeHeight);
    }

    toggleEnvironment() {
        this.config.showEnvironment = !this.config.showEnvironment;
        this.render();
    }

    zoomIn() {
        this.config.zoom *= 1.2;
        this.applyZoom();
    }

    zoomOut() {
        this.config.zoom *= 0.8;
        this.applyZoom();
    }

    resetZoom() {
        this.config.zoom = 1;
        this.applyZoom();
    }

    applyZoom() {
        this.svg.style.transform = `scale(${this.config.zoom})`;
        this.svg.style.transformOrigin = 'top left';
    }
}

// Export for use
window.TreeRenderer = TreeRenderer;