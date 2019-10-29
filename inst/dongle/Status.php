<html>
<head><title>Physics Playground Proc4 Server Status</title>
<?php
    include 'config.php';

    $mong = new MongoDB\Client("mongodb://localhost"); // connect

    function isRunning($name) {
             exec("pgrep -c $name",$found);
              return $found > 0;
    }
?>
</head>
<body>

<h1>Processes Currently Running</h1>

<p>Need to work around certain security indexes before doing this.</p>

<h2> Learning Locker to Proc 4 Daemons </h2>

<p>Download loop from NSF  <?php echo "may be " ?>running.</p>

<h2> EIEvent <h2>

<h2> EAEvent <h2>

<h1>Processed and unprocessed data </h1>

<table border="1">
<tr><th>Application</th><th>Active</th><th>Players</th>
        <th colspan=2>Evidence Identification</th><th colspan=2>Evidence Accumulation</th></tr>
<tr><th></th><th></th><th></th>
        <th>Done</th><th>Left</th><th>Done</th><th>Left</th></tr>
<?php
        foreach ($apps as $short => $id) {
                $appQuery = array ('app' => $id);
                $appRec =$mong->Proc4->AuthorizedApps->findOne(appQuery);
                $playerCount = $mong->Proc4->Players->count($appQuery);
                $EIdone = $mong->EIRecords->Events->count(['app'=>$id,'processed' => true]);
                $EIleft = $mong->EIRecords->Events->count(['app'=>$id,'processed' => false]);
                $EAdone = $mong->EARecords->EvidenceSets->count(['app'=>$id,'processed' => true]);
                $EAleft = $mong->EARecords->EvidenceSets->count(['app'=>$id,'processed' => false]);
        
?>
<tr>
   <td><?php echo $short; ?></td>
   <td><?php echo $appRec['active']; ?></td>
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

<ul>
  <li> <a href="https://pluto.coe.fsu.edu/PhysicsPlayground/ppObs.linear.csv">
          linear </a></li>
  <li> <a href="https://pluto.coe.fsu.edu/PhysicsPlayground/ppObs.adaptive.csv">
          adaptive </a></li>
  <li> <a href="https://pluto.coe.fsu.edu/PhysicsPlayground/ppObs.userControl.csv">
          userControl </a></li>

</body>
</html>