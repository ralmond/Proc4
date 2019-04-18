// This file contains javascript to set up the collections expected by
// the EI process

// Before running this script, create passwords for the Proc4
// processes you will use.
// You should put these in a file called ".Proc4.js" in your home
// directory.  It should look like:
// var pwds = [
//     {"user":"EAP","pwd":"Secret1"},
//     {"user":"ASP","pwd":"Secret2"},
//     {"user":"EIP","pwd":"Secret3"},
//     {"user":"C4","pwd":"Secret4"},
// ];
// Then load that file.  Change the next line
// To reflect the name of that path.
load("/home/ralmond/.Proc4.js")

c4User = pwds.filter(function(u) {return u.user == "C4";})[0];
c4User["roles"] = [{role:"readWrite", db:"Proc4"},
                   {role:"read", db:"EARecords"},
                   {role:"read", db:"EIRecords"},
                   {role:"read", db:"ASRecords"},
                   {role:"dbAdmin", db:"Proc4"}];
db.getSiblingDB("Proc4").createUser(c4User);

eiUser = pwds.filter(function(u) {return u.user == "EIP";})[0];
eiUser["roles"] = [{role:"readWrite", db:"EIRecords"},
                   {role:"read", db:"gameLRS"},
                   {role:"dbAdmin", db:"EIRecords"}];
db.getSiblingDB("EIRecords").createUser(eiUser);

eaUser = pwds.filter(function(u) {return u.user == "EAP";})[0];
eaUser["roles"] = [{role:"readWrite", db:"EARecords"},
                   {role:"read", db:"EIRecords"},
                   {role:"dbAdmin", db:"EARecords"}];
db.getSiblingDB("EARecords").createUser(eaUser);

asUser = pwds.filter(function(u) {return u.user == "ASP";})[0];
asUser["roles"] = [{role:"readWrite", db:"ASRecords"},
                   {role:"read", db:"EARecords"},
                   {role:"dbAdmin", db:"ASRecords"}];
db.getSiblingDB("ASRecords").createUser(asUser);

