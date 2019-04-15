<?php
if ($_SERVER['REQUEST_METHOD'] == 'GET') {
?>
<html>
<head><title>Player Start Message</title></head>
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
    $timestamp = $_POST['timestamp'];
    if (strlen($timestamp)==0)
        $timestamp = date('c');
    $P4mess = array(
       'app'             => $app,
       'uid'               => $_POST['uid'],
       'context'      => $_POST['context'],
       'sender'        => $_POST['sender'],
       'message'    => "Player Start",
       'active'         => TRUE,
       'timestamp' => $timestamp,
       'data'            => $_POST['data']
    );

    $mong = new MongoClient(); // connect
    $db=$m->selectDB("Proc4");
    
    $col=$db->selectCollection("Players");
    $col->intert($P4mess);
    
    header('Content-Type: application/json;charset=utf-8');
    echo json_encode($_POST);
    //printf("Player started: %s (%s)",$_POST['uid'],$_POST['app']);
} else {
    die("This script only works with GET and POST requests.");
}
?>

