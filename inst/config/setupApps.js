
load("/usr/local/share/Proc4/Proc4.js")


con = new Mongo();
db=con.getDB("Proc4");                 

apps.forEach(function (row) {
    db.AuthorizedApps.replaceOne(
        {"app":row.app},
        {"app":row.app,"appStem":row.appStem,
         "EIactive":false,"EIsignal":"finish",
         "EAactive":false,"EAsignal":"finish",
         "ASactive":false,"ASsignal":"finish",
         "doc":row.doc},
        {"upsert":true});
});

