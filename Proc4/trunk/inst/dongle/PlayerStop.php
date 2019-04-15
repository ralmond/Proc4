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
    if (strpos($_POST['app'],'ecd://epls.coe.fsu.edu/') != 0) {
        die("That application is not supported on this server.");
    }
    header('Content-Type: application/json;charset=utf-8');
    echo json_encode($_POST);
} else {
    die("This script only works with GET and POST requests.");
}
?>


