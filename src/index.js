import { Elm } from "./Main.elm";

const STORAGE_KEY = "__PMC__";
const flags = localStorage.getItem(STORAGE_KEY);
const app = Elm.Main.init({ flags });

app.ports.storeCache.subscribe(function (val) {
  if (val === null) {
    localStorage.removeItem(STORAGE_KEY);
  } else {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(val));
  }

  console.log("storeCache called", val);

  // Report that the new session was stored successfully.
  setTimeout(function () {
    console.log("onStoreChange called", val);
    app.ports.onStoreChange.send(val);
  }, 0);
});

app.ports.storePMModels?.subscribe(function (val) {
  if (val === null) {
    localStorage.removeItem(STORAGE_KEY);
  } else {
    const current = localStorage.getItem(STORAGE_KEY);
    const newVal = JSON.stringify({ ...JSON.parse(current), pmModels: val });
    localStorage.setItem(STORAGE_KEY, JSON.stringify(newVal));
  }

  // Report that the new session was stored successfully.
  setTimeout(function () {
    app.ports.onStoreChange.send(val);
  }, 0);
});
// Whenever localStorage changes in another tab, report it if necessary.
window.addEventListener(
  "storage",
  function (event) {
    if (event.storageArea === localStorage && event.key === STORAGE_KEY) {
      app.ports.onStoreChange.send(event.newValue);
    }
  },
  false
);
