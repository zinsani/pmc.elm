import { Elm } from "./Main.elm";

const STORAGE_KEY = "__PMC__";
function getStoredData() {
  return JSON.parse(localStorage.getItem(STORAGE_KEY));
}

const flags = localStorage.getItem(STORAGE_KEY);
console.log(JSON.parse(flags));

const app = Elm.Main.init({ flags });

app.ports.storeData.subscribe(function (data) {
  const current = getStoredData();
  const newVal = current
    ? {
        ...current,
        ...data
      }
    : data;

  localStorage.setItem(STORAGE_KEY, JSON.stringify(newVal));
});

app.ports.storeSites.subscribe(function (sites) {
  const current = getStoredData();
  const newVal = {
    ...current,
    sites,
    pmModels: current.pmModels || []
  };
  console.log("storeSites", sites);
  localStorage.setItem(STORAGE_KEY, JSON.stringify(newVal));
});

app.ports.fetch.subscribe(function () {
  const val = getStoredData();

  // Report that the new pmModels was stored successfully.
  setTimeout(function () {
    console.log("fetch", val);
    app.ports.onStoreChange.send(val);
  }, 0);
});

app.ports.storePMModels?.subscribe(function (pmModels) {
  const current = getStoredData();
  const newVal = { ...current, pmModels };
  localStorage.setItem(STORAGE_KEY, JSON.stringify(newVal));
});

app.ports.storePMModel?.subscribe(function (pmModel) {
  const current = getStoredData();
  const found = current.pmModels?.find(pm => pm.siteId == pmModel.siteId);

  console.log("storePMModel: found", found);
  const pmModels = found
    ? current.pmModels.map(md => (md.siteId === pmModel.siteId ? pmModel : md))
    : [...(current.pmModels ?? []), pmModel];

  const newVal = { ...current, pmModels };
  localStorage.setItem(STORAGE_KEY, JSON.stringify(newVal));
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
