<?php
// From https://www.linuxquestions.org/questions/programming-9/php-how-to-validate-a-password-from-htpasswd-4175589072/
// APR1-MD5 encryption method (windows compatible)
function crypt_apr1_md5($plainpasswd, $salt)
{
    $tmp = "";
    $len = strlen($plainpasswd);
    $text = $plainpasswd.'$apr1$'.$salt;
    $bin = pack("H32", md5($plainpasswd.$salt.$plainpasswd));
    for($i = $len; $i > 0; $i -= 16) { $text .= substr($bin, 0, min(16, $i)); }
    for($i = $len; $i > 0; $i >>= 1) { $text .= ($i & 1) ? chr(0) : $plainpasswd{0}; }
    $bin = pack("H32", md5($text));
    for($i = 0; $i < 1000; $i++)
    {
        $new = ($i & 1) ? $plainpasswd : $bin;
        if ($i % 3) $new .= $salt;
        if ($i % 7) $new .= $plainpasswd;
        $new .= ($i & 1) ? $bin : $plainpasswd;
        $bin = pack("H32", md5($new));
    }
    for ($i = 0; $i < 5; $i++)
    {
        $k = $i + 6;
        $j = $i + 12;
        if ($j == 16) $j = 5;
        $tmp = $bin[$i].$bin[$k].$bin[$j].$tmp;
    }
    $tmp = chr(0).chr(0).$bin[11].$tmp;
    $tmp = strtr(strrev(substr(base64_encode($tmp), 2)),
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
    "./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
 
    return "$"."apr1"."$".$salt."$".$tmp;
}

function get_htpasswd ( $passwdFile, $username )
{
    $lines = file($passwdFile);
    foreach ($lines as $line)
    {
        $arr = explode(":", $line);
        $fileUsername = $arr[0];
        if ($fileUsername == $username)
        {
            $filePasswd = trim($arr[1]);
            return $filePasswd;
        }
    }
    return false;
}

function matches($password, $filePasswd)
{
    if (strpos($filePasswd, '$apr1') === 0)
    {
        // MD5
        $passParts = explode('$', $filePasswd);
        $salt = $passParts[2];
        $hashed = crypt_apr1_md5($password, $salt);
        return $hashed == $filePasswd;
    }
    elseif (strpos($filePasswd, '{SHA}') === 0)
    {
        // SHA1
        $hashed = "{SHA}" . base64_encode(sha1($password, TRUE));
        return $hashed == $filePasswd;
    }
    elseif (strpos($filePasswd, '$2y$') === 0)
    {
       // Bcrypt
       return password_verify ($password, $filePasswd);
    }
    else
    {
        // Crypt
        $salt = substr($filePasswd, 0, 2);
        $hashed = crypt($password, $salt);
        return $hashed == $filePasswd;
    }
    return false;
}
