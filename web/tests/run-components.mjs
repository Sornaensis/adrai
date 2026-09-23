import { spawn } from "node:child_process";
import { fileURLToPath } from "node:url";
import path from "node:path";

const webRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const mode = process.argv[2] || "normal";

if (mode === "normal") {
  const executable = path.join(webRoot, "node_modules", "elm-test", "bin", "elm-test");
  const child = spawn(process.execPath, [executable, "tests/ExplorerTest.elm"], {
    cwd: webRoot, stdio: "inherit", windowsHide: true, shell: false
  });
  child.once("error", error => {
    console.error(error.message);
    process.exitCode = 1;
  });
  child.once("close", code => { process.exitCode = code ?? 1; });
} else if (["timeout", "early-success", "early-error"].includes(mode)) {
  const grandchild = spawn(process.execPath, ["-e", "setInterval(() => {}, 1000);process.stdout.write('READY\\n')"], {
    stdio: ["ignore", "pipe", "ignore"], windowsHide: true, shell: false, detached: true
  });
  grandchild.once("error", error => { console.error(error.message); process.exitCode = 1; });
  grandchild.stdout.once("data", () => {
    console.log("owned_descendant_pid=" + grandchild.pid);
    if (mode === "timeout") setInterval(() => {}, 1000);
    else process.exit(mode === "early-success" ? 0 : 17);
  });
} else {
  console.error("Unsupported component worker mode");
  process.exitCode = 1;
}
