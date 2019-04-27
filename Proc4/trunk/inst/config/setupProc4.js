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
load("/usr/local/share/Proc4/Proc4.js")

c4User = pwds.filter(function(u) {return u.user == "C4";})[0];

con = new Mongo();
db=con.getDB("Proc4");                 
db.auth(c4User.user,c4User.pwd);
db.AuthorizedApps.insert({app:"ecd://epls.coe.fsu.edu/P4test",documentation:"For system testing."});
db.createCollection("Players", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["app","uid","active","timestamp"],
            properties: {
                app: {
                    bsonType: "string",
                    description: "Application ID (string)"
                },
                uid: {
                    bsonType: "string",
                    description: "User (studnet) ID (string)"
                },
                active: {
                    bsonType: "bool",
                    description: "Is the player currently active?"
                },
                timestamp: {
                    bsonType: "date",
                    description: "Timestamp"
                }
            }
        }
    },
    validationAction: "warn"
});
db.Players.createIndex( { app:1, uid: 1});
db.createCollection("Statistics", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["app","uid","timestamp"],
            properties: {
                app: {
                    bsonType: "string",
                    description: "Application ID (string)"
                },
                uid: {
                    bsonType: "string",
                    description: "User (studnet) ID (string)"
                },
                context: {
                    bsonType: "string",
                    description: "Context (task) ID (string)"
                },
                sender: {
                    bsonType: "string",
                    description: "Who posted this message."
                },
                mess: {
                    bsonType: "string",
                    description: "Topic of message"
                },
                timestamp: {
                    bsonType: "date",
                    description: "Timestamp"
                },
                data: {
                    bsonType: "object",
                    description: "Named list of statistics."
                }
            }
        }
    },
    validationAction: "warn"
});
db.Statistics.createIndex( { app:1, uid: 1, timestamp: -1});
db.createCollection("Activities", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["app","uid","timestamp"],
            properties: {
                app: {
                    bsonType: "string",
                    description: "Application ID (string)"
                },
                uid: {
                    bsonType: "string",
                    description: "User (studnet) ID (string)"
                },
                context: {
                    bsonType: "string",
                    description: "Context (task) ID (string)"
                },
                sender: {
                    bsonType: "string",
                    description: "Who posted this message."
                },
                mess: {
                    bsonType: "string",
                    description: "Topic of Message"
                },
                timestamp: {
                    bsonType: "date",
                    description: "Timestamp"
                },
                data: {
                    bsonType: "object",
                    description: "Named list of statistics."
                }
            }
        }
    },
    validationAction: "warn"
});
db.Activity.createIndex( { app:1, uid: 1, timestamp: -1});





        
