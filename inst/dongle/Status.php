<html>
<head><title>Physics Playground Proc4 Server Status</title>
<?php
    include 'config.php';

    $mong = new MongoDB\Client("mongodb://localhost"); // connect

?>
</head>
<body>

<h1>Processes Currently Running</h1>

<table border="1">
  <tr><th>Application</th><th colspan="2">Evidence Identification</th>
    <th colspan="2">Evidence Accumulation</th>
    <th colspan="2">Activity Selection</th></tr>
  <tr><th></th><th>Active</th><th>Signal</th><th>Active</th><th>Signal</th>
    <th>Active</th><th>Signal</th></tr>
<?php
        foreach ($INI['apps'] as $short => $id) {
                $appQuery = array ('app' => $id);
                $appRec =$mong->Proc4->AuthorizedApps->findOne($appQuery);
?>
<tr>
   <td><?php echo $short; ?></td>
   <td><?php echo $appRec['EIactive']; ?></td>
   <td><?php echo $appRec['EIsignal']; ?></td>
   <td><?php echo $appRec['EAactive']; ?></td>
   <td><?php echo $appRec['EAsignal']; ?></td>
   <td><?php echo $appRec['ASactive']; ?></td>
   <td><?php echo $appRec['ASsignal']; ?></td>


</tr>
<?php 
}
?>
</table>

<?php
/* Old Method */
/* I'm currently having difficulty figuring out how to run shell
 * commands to check if the various proceses are running.  My current
 * strategy is to run the script p4Status as a cron job sending it's
 * output to P4Status.html in the current directory.  */
//include("P4Status.html"); 


?>

<h1>Processed and unprocessed data </h1>

<table border="1">
<tr><th>Application</th><th>Players</th>
        <th colspan="2">Evidence Identification</th><th colspan="2">Evidence Accumulation</th></tr>
<tr><th></th><th></th>
        <th>Done</th><th>Left</th><th>Done</th><th>Left</th></tr>
<?php
        foreach ($INI['apps'] as $short => $id) {
                $appQuery = array ('app' => $id);
                $appRec =$mong->Proc4->AuthorizedApps->findOne($appQuery);
                $playerCount = $mong->Proc4->Players->count($appQuery);
                $EIdone = $mong->EIRecords->Events->count(['app'=>$id,'processed' => true]);
                $EIleft = $mong->EIRecords->Events->count(['app'=>$id,'processed' => false]);
                $EAdone = $mong->EARecords->EvidenceSets->count(['app'=>$id,'processed' => true]);
                $EAleft = $mong->EARecords->EvidenceSets->count(['app'=>$id,'processed' => false]);
        
?>
<tr>
   <td><?php echo $short; ?></td>
   <td><?php echo $playerCount; ?></td>
   <td><?php echo $EIdone; ?></td>
   <td><?php echo $EIleft; ?></td>
   <td><?php echo $EAdone; ?></td>
   <td><?php echo $EAleft; ?></td>
</tr>
<?php 
}
?>
</table>

<h1>Links to results files</h1>

<h2>Evidence Identification</h2>

<table border="1">
<tr><th>Application</th><th>Name</th><th>Timestamp</th><th>doc</th></tr>
<?php
        $fileQuery = array ('process' => 'EI', 'type' => 'data');
$cursor =$mong->Proc4->OutputFiles->find($fileQuery);
foreach($cursor as $fileRec) {
?>
<tr>
    <td><?php echo basename($fileRec['app']); ?></td>
    <td><?php echo '<a href="logs/'.basename($fileRec['filename']).'"</a>'; ?><?php echo $fileRec['name']; ?></a></td>
    <td><?php echo $fileRec['timestamp']; ?></td>
    <td><?php echo $fileRec['doc']; ?></td>
</tr>
<?php 
}
?>
</table>



<h2>Evidence Accumulation</h3>

<table border="1">
<tr><th>Application</th><th>Name</th><th>Timestamp</th><th>doc</th></tr>
<?php
        $fileQuery = array ('process' => 'EA', 'type' => 'data');
$cursor =$mong->Proc4->OutputFiles->find($fileQuery);
foreach($cursor as $fileRec) {
?>
<tr>
    <td><?php echo basename($fileRec['app']); ?></td>
    <td><?php echo '<a href="logs/'.basename($fileRec['filename']).'"</a>'; ?><?php echo $fileRec['name']; ?></a></td>
   <td><?php echo $fileRec['timestamp']; ?></td>
   <td><?php echo $fileRec['doc']; ?></td>
</tr>
<?php 
}
?>
</table>

<h1>Links to log files</h1>

<h2>Evidence Identification</h2>

<table border="1">
<tr><th>Application</th><th>Name</th><th>Timestamp</th><th>doc</th></tr>
<?php
        $fileQuery = array ('process' => 'EI', 'type' => 'log');
$cursor =$mong->Proc4->OutputFiles->find($fileQuery);
foreach($cursor as $fileRec) {
?>
<tr>
    <td><?php echo basename($fileRec['app']); ?></td>
    <td><?php echo '<a href="data/'.basename($fileRec['filename']).'"</a>'; ?><?php echo $fileRec['name']; ?></a></td>
   <td><?php echo $fileRec['timestamp']; ?></td>
   <td><?php echo $fileRec['doc']; ?></td>
</tr>
<?php 
}
?>
</table>



<h2>Evidence Accumulation</h3>

<table border="1">
<tr><th>Application</th><th>Name</th><th>Timestamp</th><th>doc</th></tr>
<?php
        $fileQuery = array ('process' => 'EA', 'type' => 'log');
$cursor =$mong->Proc4->OutputFiles->find($fileQuery);
foreach($cursor as $fileRec) {
?>
<tr>
    <td><?php echo basename($fileRec['app']); ?></td>
    <td><?php echo '<a href="data/'.basename($fileRec['filename']).'"</a>'; ?><?php echo $fileRec['name']; ?></a></td>
   <td><?php echo $fileRec['timestamp']; ?></td>
   <td><?php echo $fileRec['doc']; ?></td>
</tr>
<?php 
}
?>
</table>


<h2>Shutdow Page</h2>
<p> Here is the <a href=Shutdown.php">shutdown page.</a></p>

</body>
</html>
