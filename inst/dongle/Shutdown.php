<?php

if ($_SERVER['REQUEST_METHOD'] == 'GET' || $_SERVER['REQUEST_METHOD'] == 'POST' ) {
?>
<html>
<head><title>Shutdown server process</title></head>
<body>

    <p> This is an interface to the Proc4 system.
    For more information about Proc4, go to
        <a href="https://pluto.coe.fsu.edu/Proc4/">Proc 4 home
    page.</a></p>

    <p> This form will signal the specified process to take one of the
    following actions:</p> 
    <dl>
      <dt>running</dt><dd>Continue running (no effect; Note this will
        not restart the process.)</dd> 
      <dt>finish</dt><dd>Stop processing after all messsages in the
        database have been processed.</dd>  
      <dt>halt</dt><dd>Stop processing as soon as the current message
        is finished processing.</dd> 
    </dl>

    <form action="<?php echo $_SERVER['PHP_SELF'] ?>" method="POST">
      App: <input type="url" id="app" name="app" pattern="ecd://.*" required/><br/>
      Process: <select id="process" name="process" required>
        <option value="EI">Evidence Identification (EI)</option>
        <option value="EA">Evidence Accumulation (EA)</option>
        <option value="AS">Activity Selection (AS)</option>
      </select>  <br/>
      Signal: <select id="signal" name="signal">
        <option selected>running</option>
        <option>finish</option>
        <option>halt</option>
      </select>  <br/>
      Confirm Halt: <input type="checkbox" id="shouldHalt" name="shouldHalt" value="shouldHalt"/><br/>
      Administrator: <input type="text" id="aid" name="aid"/ required><br/>
      Password: <input type="password" id="pwd" name="pwd"/ required><br/>
      <input type="submit" name="Do it!"/>
    </form>
    <?php
    if ($_SERVER['REQUEST_METHOD'] == 'POST')
    {
        include 'config.php';
        include 'checkPwd.php';

        $mong = new MongoDB\Client("mongodb://localhost"); // connect

        $app = $_POST['app'];
        if (strpos($app,'ecd://epls.coe.fsu.edu/') != 0) {
            die("That application is not supported on this server.");
        }

        if (!in_array($app,$INI['apps'])) {
            die("App '$app' not registered.");
        }
        $aid = $_POST['aid'];
        $pwd = $_POST['pwd'];
        $filepwd = get_htpasswd('/usr/local/share/Proc4/p4pwds',$aid);
        if (!matches($pwd,$filepwd)) {
            die("Username or password not matched.");
        }
        $proc = $_POST['process'];
        $signal = $_POST['signal'];
        if ($signal == 'halt' && !array_key_exists('shouldHalt',$_POST))
            die("Halt selected but not confirmed.");
        

        $mong = new MongoDB\Client("mongodb://localhost"); 
        
        $col=$mong->Proc4->AuthorizedApps;
        $rec=$col->findOne(['app' => $app],['limit' => 1]);
        if ($rec == null) {
            die("No record for process.");
        } else {
            $result = $col->updateOne(['app' => $app],
                                      ['$set' => [$proc.'signal' => $signal]]);
        }    

    ?>
    <p><?php echo $proc." process for ".$app." sent signal ".$signal."."; ?></p>
    
<?php
}
?>

<h2>Links to Other Pages</h2>
<ul>
    <li> <a href=Status.php">status</a> page.</li>
    <li> <a href=Shutdown.php">Shutdown</a> page.</li>
    <li> <a href=EIBuilder.php">Evidence Identification (EI)
        Builder (Loader)</a>.</li>
    <li> <a href=EABuilder.php">Evidence Accumulation (EA)
        Net Builder</a>.</li>
    <li> <a href=EIEvent.php">Evidence Identification (EI)
        Launcher</a>.</li>
    <li> <a href=EABN.php">Evidence Accumulation (EA)
        Launcher</a>.</li>
</ul>
</body>
</html>

<?php
} elseif($_SERVER['REQUEST_METHOD']=="OPTIONS") {
    header('Access-Control-Allow-Credentials: true');
    header('Access-Control-Allow-Headers: Content-Type, Accept, access-control-allow-credentials, access-control-allow-headers, access-control-allow-methods, access-control-allow-origin, access-control-max-age, X-Access-Token, X-Application-Name, X-Request-Time, X-Powered-by');
    header('Access-Control-Allow-Methods: GET, POST, OPTIONS');
    header('Access-Control-Allow-Origin: *');
    header('Access-Control-Max-Age: 1728000');
    header('Content-Length: 0');
    header('Content-Type: text/plain');
} else {
    die("This script only works with GET and POST requests.");
}
?>

