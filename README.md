# Proc4
Four Process/Mongo integration in R

This is a utility package which defines common structures for the EI(Event) and EA(BN) packages.

This includes:

1) The definition of the basic P4 message object.
2) The definition of Listener objects to record events in the database (or other sink)
3) Logging and error handling facilities (extensions to `futile.logger`).
4) Scripts for setting up Mongo databases to work with EI, EA and AS processes.
5) A PHP "Dongle" which can be used to connect a game or simulation engine to the Mongo database.

Work on the Proc4, EIEvent and EABN packages has been supported by the
National Science foundation grants *DIP: Game-based Assessment and
Support of STEM-related Competencies* (\#1628937, Val Shute, PI) and
*Mathematical Learning via Architectual Design and Modeling Using
E-Rebuild.* (\#1720533, Fengfeng Ke, PI).
