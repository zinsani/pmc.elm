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

app.ports.storeSite?.subscribe(function (newSite) {
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

app.ports.storePlayerManagers?.subscribe(function (playerManagers) {
  const current = getStoredData();
  const siteList = current.list.map(site => ({...site, playerManagers: site.playerManagers.filter(pmId => playerManagers.includes(p => p.id === pmId))}));
    
  const newVal = { ...current, sites: { ...sites, list: siteList}, playerManagers };
  localStorage.setItem(STORAGE_KEY, JSON.stringify(newVal));
});

app.ports.storePlayerManager.subscribe(function ([siteId, playerManager]) {
    console.log("storePlayerManager", siteId, playerManager);

  const current = getStoredData();
  const siteList = current.sites.list.map(site => site.id === siteId ? 
        { ... site, 
            lastInputPM: {...playerManager, id: "temp" }, 
            playerManagers: site.playerManagers.includes(playerManager.id) ? 
            site.playerManagers 
            : [...site.playerManagers, playerManager.id]} 
        : site);

  const playerManagers = current.playerManagers.find(pm => pm.id === playerManager.id) ? 
        current.playerManagers.map(pm => pm.id ===playerManager.id ? 
            playerManager 
            : pm) 
        : [...current.playerManagers, playerManager]; 

  console.log('new player managers', playerManagers);

  const newVal = { ...current, sites: {...current.sites, list: siteList }, playerManagers };
  localStorage.setItem(STORAGE_KEY, JSON.stringify(newVal));
});

app.ports.removePlayerManager.subscribe(function ([siteId, pmIdToRemove]) {
    console.log("removePlayerManager", siteId, pmIdToRemove);

  const current = getStoredData();
  const siteList = current.sites.list.map(site => site.id === siteId ? 
        { ... site, 
            playerManagers: site.playerManagers.filter(pmId => pmId !== pmIdToRemove)} 
        : site);

  const playerManagers = current.playerManagers.filter(pm => pm.id !== pmIdToRemove) 

  console.log('new player managers', playerManagers);

  const newVal = { ...current, sites: {...current.sites, list: siteList }, playerManagers };
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
