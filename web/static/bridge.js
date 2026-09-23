(() => {
  "use strict";
  const credential = typeof window.__ADRAI_TOKEN__ === "string" && window.__ADRAI_TOKEN__
    ? window.__ADRAI_TOKEN__ : null;
  delete window.__ADRAI_TOKEN__;

  const readPaths = new Set([
    "/api/v1/repository", "/api/v1/search", "/api/v1/relevant",
    "/api/v1/history", "/api/v1/compare", "/api/v1/conflicts", "/api/v1/doctor"
  ]);
  const adrRead = /^\/api\/v1\/adrs\/[A-Za-z0-9-]+$/;
  const writePath = /^\/api\/v1\/adrs(?:\/[A-Za-z0-9-]+\/(?:amend|scope|domain|obsolete|reactivate))?$/;
  const socketPath = "/api/v1/events";
  let socket = null;
  let interests = [];
  let app = null;

  function report(payload) {
    if (app) app.ports.fromJs.send(payload);
  }

  function validPath(method, path) {
    if (typeof path !== "string" || path.length > 8192 ||
        !path.startsWith("/api/v1/") || path.includes("\\") ||
        path.includes("#") || path.includes("//")) return false;
    let url;
    try { url = new URL(path, window.location.origin); } catch { return false; }
    if (url.origin !== window.location.origin ||
        url.pathname !== path.split("?")[0] ||
        url.search.length > 4096) return false;
    if (method === "GET") return readPaths.has(url.pathname) || adrRead.test(url.pathname);
    return method === "POST" && !url.search && writePath.test(url.pathname);
  }

  async function request(command) {
    const id = command.request_id;
    if (typeof id !== "string" || id.length > 128 ||
        !validPath(command.method, command.path)) {
      report({ type: "request-failed", request_id: String(id || ""), message: "Invalid API request." });
      return;
    }
    if (command.method === "POST" && !credential) {
      report({ type: "request-failed", request_id: id, message: "Reopen the process bootstrap URL to submit changes." });
      return;
    }
    const headers = {};
    const options = { method: command.method, credentials: "same-origin", headers };
    if (command.method === "POST") {
      headers["Content-Type"] = "application/json";
      headers.Authorization = "Bearer " + credential;
      options.body = JSON.stringify(command.body);
    }
    try {
      const response = await fetch(command.path, options);
      const body = await response.json();
      report({ type: "response", request_id: id, status: response.status, body });
    } catch {
      report({ type: "request-failed", request_id: id, message: "Network or JSON response failed. Inspect repository state before retrying a mutation." });
    }
  }

  function validInterests(paths) {
    return Array.isArray(paths) && paths.length <= 32 &&
      paths.every(path => typeof path === "string" && path.length > 0 &&
        path.length <= 1024 && !path.startsWith("/") && !path.includes("\\") &&
        !path.split("/").some(part => !part || part === "." || part === "..")) &&
      new Set(paths).size === paths.length;
  }

  function sendInterests() {
    if (socket && socket.readyState === WebSocket.OPEN)
      socket.send(JSON.stringify({ type: "active-files", paths: interests }));
  }

  function disconnect() {
    interests = [];
    if (socket) {
      const previous = socket;
      socket = null;
      if (previous.readyState === WebSocket.OPEN)
        previous.send(JSON.stringify({ type: "active-files", paths: [] }));
      previous.close();
    }
    report({ type: "socket-state", state: "closed" });
  }

  function connect() {
    if (!credential) {
      report({ type: "socket-state", state: "unavailable" });
      return;
    }
    if (socket && (socket.readyState === WebSocket.OPEN || socket.readyState === WebSocket.CONNECTING))
      return;
    report({ type: "socket-state", state: "connecting" });
    const scheme = window.location.protocol === "https:" ? "wss:" : "ws:";
    const current = new WebSocket(scheme + "//" + window.location.host + socketPath);
    socket = current;
    current.onopen = () => {
      if (socket !== current) return;
      current.send(JSON.stringify({ type: "authenticate", credential }));
      sendInterests();
      report({ type: "socket-state", state: "open" });
    };
    current.onmessage = event => {
      if (socket !== current || typeof event.data !== "string") return;
      try { report({ type: "event", body: JSON.parse(event.data) }); }
      catch { report({ type: "socket-state", state: "unavailable" }); disconnect(); }
    };
    current.onerror = () => {
      if (socket === current) report({ type: "socket-state", state: "unavailable" });
    };
    current.onclose = event => {
      if (socket !== current) return;
      socket = null;
      if (event.reason === "generation-exhausted; restart the web server")
        report({ type: "socket-state", state: "unavailable", reason: "generation-exhausted" });
      else
        report({ type: "socket-state", state: "closed" });
    };
  }

  function command(value) {
    if (!value || typeof value !== "object") return;
    switch (value.type) {
      case "request": request(value); break;
      case "connect": connect(); break;
      case "disconnect": disconnect(); break;
      case "active-files":
        if (validInterests(value.paths)) {
          interests = value.paths.slice();
          sendInterests();
        }
        break;
    }
  }

  function mount() {
    const node = document.getElementById("adrai-app");
    if (!node || !window.Elm || !window.Elm.Main) return;
    app = window.Elm.Main.init({ node, flags: { hasCredential: !!credential } });
    app.ports.toJs.subscribe(command);
  }

  if (document.readyState === "loading")
    document.addEventListener("DOMContentLoaded", mount, { once: true });
  else
    mount();
})();
