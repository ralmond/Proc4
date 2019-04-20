<?php
// This file contains php to load configuration details.
// It gets its information from:
$PROC4_ETC ="/usr/local/share/Proc4/";

// In there should be a file named Proc4.ini
// It should look something like:
// [apps]
// test = "ecd://epls.coe.fsu.edu/P4test"

// [users]
// EIP = "secret"
// EAP = "secret"
// ASP = "secret"
// C4 = "secret"

$INI = parse_ini_file("$PROC4_ETC/Proc4.ini",true);

require_once __DIR__."mongodb/autoload.php";

?>