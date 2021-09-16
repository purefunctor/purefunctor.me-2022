const fs = require("fs/promises");
const nunjucks = require("nunjucks");

nunjucks.configure(["html"]);

async function main() {
    await fs.mkdir("./public", { recursive: true });
    let pages = await fs.readdir("html/pages");
    pages.forEach(async page => {
	let result = nunjucks.render(`./pages/${page}`);
	await fs.writeFile(`./public/${page}`, result);
    });
}

main();
