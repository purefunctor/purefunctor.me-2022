const chrome = require("chrome-aws-lambda");
const puppeteer = require("puppeteer-core");

async function getScreenshot(url) {
  const browser = await puppeteer.launch({
    args: chrome.args,
    executablePath: await chrome.executablePath,
    headless: chrome.headless,
  });
  const page = await browser.newPage();
  await page.goto(url);
  await page.setViewport({ width: 1200, height: 630 });
  return await page.screenshot();
};

module.exports = (_, res) => {
  getScreenshot("https://purefunctor.me").then(
    file => {
      res.setHeader(
        "Content-Type",
        "image/png",
      );
      res.setHeader(
        "Cache-Control",
        "public, immutable, no-transform, s-maxage=31536000, max-age=31536000",
      );
      res.send(file);
    }
  ).catch(
    error => {
      res.status(500);
      res.json({"message": "an error was encountered internally!" + `${error}`});
    }
  );
};
