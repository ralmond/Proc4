<?php
if ($_SERVER['REQUEST_METHOD'] == 'GET') {
?>
<html>
<head><title>Player Start Message</title></head>
<body>

    <p> This is a test interface for the Proc4 system.
    It will echo the message recieved as json.
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
    $ecdapp = strpos($app,'ecd://epls.coe.fsu.edu');
    if ($ecdapp === false ||  $ecdapp != 0) {
        die("That application is not supported on this server.");
    } else {
        include 'config.php';
        if (in_array($app,$INI['apps'])) {
            header('Content-Type: application/json;charset=utf-8');
            echo json_encode($_POST);
        } else {
            die("App '$app' not initialized.");
        }
    }
} else {
    die("This script only works with GET and POST requests.");
}
?>
