<?php
if ($_SERVER['REQUEST_METHOD'] == 'GET') {
?>
<html>
<head><title>Player Stop Message</title></head>
<body>

    <p> This is an interface to the Proc4 system.
    For more information about Proc4, go to
        <a href="https://pluto.coe.fsu.edu/Proc4/">Proc 4 home
    page.</a></p>

    <form action="<?php echo $_SERVER['PHP_SELF'] ?>" method="POST">
        App: <input type="text" name="app"/><br/>
        Uid: <input type="text" name="uid"/><br/>
        Context: <input type="text" name="context"/><br/>
        Sender: <input type="text" name="sender"/><br/>
        Message: <input type="text" name="mess"/><br/>
        Timestamp: <input type="text" name="timestamp"/><br/>
        data: <input type="text" name="data"/><br/>
        <input type="submit" name="Postit!"/>
    </form>
</body>
</html>
<?php
} elseif ($_SERVER['REQUEST_METHOD'] == 'POST')
{
    $app = $_POST['app'];
    if (strpos($app,'ecd://epls.coe.fsu.edu/') != 0) {
        die("That application is not supported on this server.");
    }
    include 'config.php';
    if (!in_array($app,$INI['apps'])) {
        die("App '$app' not registered.");
    }
    $timestamp = $_POST['timestamp'];
    if (strlen($timestamp)==0)
        $timestamp = date('c');
    $uid = $_POST['uid'];
    $data = $_POST['data'];
    $P4mess = array(
       'app'       => $app,
       'uid'       => $uid,
       'context'   => $_POST['context'],
       'sender'    => $_POST['sender'],
       'message'   => "Player Stop",
       'active'    => FALSE,
       'timestamp' => $timestamp,
       'data'      => $data,
    );

    $mong = new MongoDB\Client("mongodb://localhost"); // connect
    
    $col=$mong->Proc4->Players;
    $rec=$col->findOne(['app' => $app, 'uid' => $uid],
                       ['limit' => 1,'sort' => ['timestamp' => -1]]);
    if ($rec == null) {
        $result = $col->insertOne($P4mess);
    } else {
        $result = $col->updateOne(['app' => $app, 'uid' => $uid],
        ['$set' => ['active' => false, 'timestamp' => $timestamp,
                    'data' => $data]]);
    }    

    $P4mess['message'] = "Player Stop Acknolwedge";
    $P4mess['sender'] = "Proc 4 dongle";
    header('Content-Type: application/json;charset=utf-8');
    echo json_encode($P4mess);
    //printf("Player stoped: %s (%s)",$_POST['uid'],$_POST['app']);
} elseif($_SERVER['REQUEST_METHOD']=="OPTIONS") {
    header('Access-Control-Allow-Credentials: true');
    header('Access-Control-Allow-Headers: Content-Type, Accept, X-Access-Token, X-Application-Name, X-Request-Time, X-Powered-by');
    header('Access-Control-Allow-Methods: GET, POST, OPTIONS');
    header('Access-Control-Allow-Origin: *');
    header('Access-Control-Max-Age: 1728000');
    header('Content-Length: 0');
    header('Content-Type: text/plain');
} else {
    die("This script only works with GET and POST requests.");
}
?>


