### Implementing the "cd -" command ###

Do you remember your first command in the PowerShell? Was it

```
cd ToSomeWhereInTheSystem
```

? :)
If you are familiar with the bash you may know the command
```
cd -
```
which brings you back to the path you are standing before the last directory change. Do you miss this feature in the PowerShell? So read on. We will implement and extend it.

The first task aims at replacing the built-in cd command with our code. But what's behind the cd command at the moment? Let's ask the PowerShell.
```
>get-command cd

CommandType     Name    Definition
-----------     ----    ----------
Alias           cd      Set-Location
```
Ok, it's an alias for the Set-Location command. But where will we place our code? The PowerShell attempts to resolve commands in the following order:

  * aliases
  * functions
  * cmdlets
  * scripts
  * executables
  * normal files

In my opinion an alias is designated for enclosing a single command. But a function can hold some more code. So, let's write our new cd function.
I created a new file (name "extended\_cd.ps1") for this task inside MyDocuments\WindowsPowerShell. Thats why i can easily "dot source" it with
```
. $(([System.Io.FileInfo]$profile).DirectoryName+"\extended_cd.ps1")
```
out of my $profile. (Don't be affraid of this term. Just strip it and test at the prompt, how it works.)

Rember: our task at the moment is, to replace the built-in cd command. Thats why our file should start with this command:
```
remove-item alias:cd
```
But this isn't so great as it looks, because it will show an error if there is not cd alias. An if-statement will solve this problem:
```
if ((get-alias cd 2>$null) -ne $null)
{
  remove-item alias:cd
}
```
"2>$null" means: Redirect the error messages to the data nirvana (in linux known as /dev/null).

Now we can start the work on our new cd function. Let's build the raw function and test, if it works.
```
function cd
{
  $param=[String]$args[0]
  "You passed "+$param+" as param 0" | out-host
}
```
Resource your profile (if you've done it like me easily type ". $profile") and test it.

In the next step we will implement the basic functionality of the function. We have to decide whether our special param (-) is supplied or not. If not we have to set the new location supplied in $args[0](0.md). Let's try it:
```
function cd
{
  $param=[String]$args[0]


  if ($param.StartsWith("-"))
  {
     "Olee we are alive!" | out-host
  }
  else
  {
    set-location $param
  }
}
```
Dont forget to resource your profile and test it.

In order to do our native task, we have to store the current location before changing it. But where to? Thanking the power of this shell :) we can use an ArrayList. We will declare it global, in order to access it later. Just write in front of the function the following line:
```
$_pathstack=new-object System.Collections.ArrayList
```
This creates an new object which will hold the locations later.
Now, we can modify our function. Before we set the new location, we have to store the current one and if setting the new one was successful ($? -eq $true), we will add the old location to our path stack. The whole file should looks as follows:
```
if ((get-alias cd 2>$null) -ne $null)
{
  remove-item alias:cd
}


$_pathstack=new-object System.Collections.ArrayList


function cd
{
  $param=[String]$args[0]
  

  if ($param.StartsWith("-"))
  {
     "Olee we are alive!" | out-host
  }
  else
  {
    $temp=(get-location).Path
    set-location $param
    if ($? -eq $true)
    {
      $_pathstack.add($temp) | Out-Null
    }
  }
}
```
Thats all about saving the location. Easy, isn't it?

The next task is to react on the minus parameter. If it is supplied, we have to get the last item stored in the path stack, write it to the console and set the new location. Afterwards we delete the item from the array.
```
function cd
{
  $param=[String]$args[0]
  

  if ($param.StartsWith("-"))
  {
    $index_in_pathstack=$_pathstack.count-1

    $newlocation=$_pathstack.item($index_in_pathstack)

    $newlocation | out-host
    set-location $newlocation
    
    $_pathstack.RemoveAt($index_in_pathstack)
  }
  else
  {
    $temp=(get-location).Path
    set-location $param
    if ($? -eq $true)
    {
      $_pathstack.add($temp) | Out-Null
    }
  }
}
```
At this moment we could stop development because we have reached even more then the linux shell provides. Under linux the shell stores only one path in the internal stack. Our code stores all pathes we reached and we can go all steps back.
```
C:\>cd windows
C:\WINDOWS>cd system32
C:\WINDOWS\system32>cd 1031
C:\WINDOWS\system32\1031>cd -
C:\WINDOWS\system32
C:\WINDOWS\system32>cd -
C:\WINDOWS
C:\WINDOWS>cd -
C:\
C:\>
```
But would it be fine if we can determine how many steps we want to go back? In other words: dont you dream about "cd -3" for the example above? Yes? Lets do it.

At first we have to check if a number was supplied and if it is like this, we try to cast it into a integer (by default we use 1):
```
if ($param.Length -gt 1)
{
  $cnt=[int]$param.SubString(1,$param.Length-1)
}
else
{
  $cnt=1
}
```
Of course we may catch errors from the typecast. But this will be a task for your personal studies. :)
In the next step we assure that we find an item for the supplied back step count (positive value and not greater then the length of the list).
```
$cnt=[System.Math]::Abs($cnt)
if ($cnt -gt $_pathstack.count)
{
  $cnt=1
}
```
And the rest is simple.
```
$index_in_pathstack=$_pathstack.count-$cnt

$newlocation=$_pathstack.item($index_in_pathstack)

[String]$cnt+" step(s) back to "+$newlocation | out-host
set-location $newlocation

for($idx=$_pathstack.count-1; $idx -ge $index_in_pathstack; $idx--)
{
  $_pathstack.RemoveAt($idx)
}
```


The whole result should look like this:
```
if ((get-alias cd 2>$null) -ne $null)
{
  remove-item alias:cd
}

$_pathstack=new-object System.Collections.ArrayList

function cd
{
  $param=[String]$args[0]

  if ($param.StartsWith("-"))
  {
    if ($_pathstack.count -gt 0)
    {
      if ($param.Length -gt 1)
      {
        $cnt=[int]$param.SubString(1,$param.Length-1)
      }
      else
      {
        $cnt=1
      }

      $cnt=[System.Math]::Abs($cnt)
      if ($cnt -gt $_pathstack.count)
      {
        $cnt=1
      }

      $index_in_pathstack=$_pathstack.count-$cnt

      $newlocation=$_pathstack.item($index_in_pathstack)

      [String]$cnt+" step(s) back to "+$newlocation | out-host
      set-location $newlocation

      for($idx=$_pathstack.count-1; $idx -ge $index_in_pathstack; $idx--)
      {
        $_pathstack.RemoveAt($idx)
      }
    }
    else
    {
      write-host "path stack is empty" -foregroundcolor Red
    }

  }
  else
  {
    $temp=(get-location).Path
    set-location $param
    if ($? -eq $true)
    {
      $_pathstack.add($temp) | Out-Null
    }

  }
}

```

It remains to say: Have fun! :)