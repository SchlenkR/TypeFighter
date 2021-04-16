
const puppeteer = require('puppeteer');
const fs = require('fs');
const path = require('path');

const parseDataUrl = (dataUrl) => {
    const matches = dataUrl.match(/^data:(.+);base64,(.+)$/);
    if (matches.length !== 3) {
        throw new Error('Could not parse data URL.');
    }
    return { mime: matches[1], buffer: Buffer.from(matches[2], "base64") };
};

(async () => {
    const browser = await puppeteer.launch();
    const page = await browser.newPage();
    const graphUrl = path.join(__dirname, 'web/graph.html');
    //console.log(graphUrl);
    await page.goto(`file://${graphUrl}`, {
        waitUntil: "networkidle2" // ensures images are loaded
    });

    const imageData = await page.evaluate(() => {
        myDiagram.animationManager.stopAnimation();
        return myDiagram.makeImageData({
            background: myDiagram.div.style.backgroundColor
        });
    });

    const { buffer } = parseDataUrl(imageData);
    fs.writeFileSync('diagram.png', buffer, 'base64');

    //await page.screenshot({ path: 'page-screenshot.png' });
    await browser.close();
})();
