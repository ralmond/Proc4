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
//load("/usr/local/share/Proc4/Proc4.js")

//c4User = pwds.filter(function(u) {return u.user == "C4";})[0];

con = new Mongo();
db=con.getDB("Proc4");                 
//db.auth(c4User.user,c4User.pwd);
db.AuthorizedApps.drop();
db.createCollection("AuthorizedApps", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["app","appStem"],
            properties: {
                app: {
                    bsonType: "string",
                    description: "Application ID (guid; string)"
                },
                appStem: {
                    bsonType: "string",
                    description: "Short display name for application ID (string)"
                },
                EIactive: {
                    bsonType: "bool",
                    description: "Is EI process currently active?"
                },
                EIsignal: {
                    bsonType: "string",
                    description: "One of 'running', 'finish' or 'halt'.  Used to signal the EI process to stop.",
                    enum: ["running","finish","halt"]
                },
                EAactive: {
                    bsonType: "bool",
                    description: "Is EA process currently active?"
                },
                EAsignal: {
                    bsonType: "string",
                    description: "One of 'running', 'finish' or 'halt'.  Used to signal the EA process to stop.",
                    enum: ["running","finish","halt"]
                },
                ASactive: {
                    bsonType: "bool",
                    description: "Is AS process currently active?"
                },
                ASsignal: {
                    bsonType: "string",
                    description: "One of 'running', 'finish' or 'halt'.  Used to signal the AS process to stop.",
                    enum: ["running","finish","halt"]
                },
                doc: {
                    bsonType: "string",
                    description: "Descrition of the application."
                }
            }
        }
    },
    validationAction: "warn"
});


// apps.forEach(function (row) {
//     db.AuthorizedApps.replaceOne({"app":row.app},row,{"upsert":true});
// });
db.Players.drop();
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
                context: {
                    bsonType: "string",
                    description: "Context (task) ID (string)"
                },
                timestamp: {
                    bsonType: "date",
                    description: "Timestamp"
                },
                data: {
                    bsonType: "object",
                    description: "Player State information passed to game engine at login."
                }
            }
        }
    },
    validationAction: "warn"
});
db.Players.createIndex( { app:1, uid: 1});
db.Statistics.drop();
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
db.Activities.drop();
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




db.OutputFiles.drop();
db.createCollection("OutputFiles", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["app","process","name","type","filename"],
            properties: {
                app: {
                    bsonType: "string",
                    description: "Application ID (guid; string)"
                },
                process: {
                    bsonType: "string",
                    description: "Which process generated the file."
                },
                type: {
                    bsonType: "string",
                    description: "Is this a data or log file?",
                    enum:["data","log"]
                },
                name: {
                    bsonType: "string",
                    description: "A label for the file contents."
                },
                filename: {
                    bsonType: "string",
                    description: "Name of the File."
                },
                timestamp: {
                    bsonType: "date",
                    description: "Timestamp"
                },
                doc: {
                    bsonType: "string",
                    description: "Descrition of the application."
                }
            }
        }
    },
    validationAction: "warn"
});

        
