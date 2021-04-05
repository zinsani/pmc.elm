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
    playerManagers: current.playerManagers || []
  };
  console.log("storeSites", sites);
  localStorage.setItem(STORAGE_KEY, JSON.stringify(newVal));
});

app.ports.storeSite.subscribe(function (newSite) {
  const current = getStoredData();
  const found = current.sites?.list.find(s => s.id == newSite.id);

  console.log("Site: found", found);
  const list = found
    ? current.sites.list.map(site => (site.id === newSite.id ? newSite : site))
    : [...(current.sites.list ?? []), newSite];

  const newVal = {...current, sites: { ...current.sites, list }};
  localStorage.setItem(STORAGE_KEY, JSON.stringify(newVal));
});


app.ports.fetch.subscribe(function () {
  const val = getStoredData();

  // Report that the new playerManagers was stored successfully.
  setTimeout(function () {
    console.log("fetch", val);
    app.ports.onStoreChange.send(val);
  }, 0);
});

app.ports.storePlayerManagers.subscribe(function (playerManagers) {
  const current = getStoredData();
  const newVal = { ...current, playerManagers };
  localStorage.setItem(STORAGE_KEY, JSON.stringify(newVal));
});

app.ports.storePlayerManager.subscribe(function (playerManager) {
    console.log("storePlayerManager", playerManager);

  const current = getStoredData();
  const playerManagers = current.playerManagers.find(pm => pm.id === playerManager.id) ? current.playerManagers.map(pm => pm.id ===playerManager.id ? playerManager : pm) : [...current.playerManagers, playerManager]; 

  console.log('new player managers', playerManagers);

  const newVal = { ...current, playerManagers };
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
