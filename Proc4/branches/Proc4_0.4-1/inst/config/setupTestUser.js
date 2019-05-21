
db.getSiblingDB("test");
db.createUser({user:"test",pwd:"secret",
               roles:[{role:"readWrite", db:"test"},
                      {role:"dbAdmin", db:"test"}]});
db.auth({user:"test",pwd:"secret"});
db.createCollection("Messages")
db.Messages.createIndex( { app:1, uid: 1, timestamp:-1});
