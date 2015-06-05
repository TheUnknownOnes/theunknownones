### What is TUOScriptPak? ###

TUOScriptPak is a extension for the Delphi 2006 IDE and ships with some script-functions, which can be used in live-templates. Some information about live-templates and scripts can be read at the bottom of this page.

### Which functions/templates are in this archive? ###

The following live-templates can be found:
> `needa`
> > Inserts a new component onto the current form
> > ->Used TUOScript-functions: InsertComponent

Actually, the following functions are implemented:

> `InsertComponent(ComponentClass)`
> > Inserts a new component onto the current form
> > ->parameter:
> > > ComponentClass : String //Defines the classname of the new component

> > ->live-templates using this function: needa.xml

### How can i use the functions/templates? ###

First, copy all files from the bpl-folder of the archive in a directory on your harddisk. Then, install the bpl-files in delphi via "Components"->"Intall package".

The easiest way to use the functions is, to use the shipped live-templates (**.xml-files in the folders "deutsch" & "english"). In order to do this, copy the files of your choice into your template-directory. Usally it can be found at "BDS\4.0\Objrepos\code\_templates\delphi" or at your personal folder "local settings\applicationdata\BDS\4.0\code\_templates". After this, the new templates will occure in the template-list of delphi ("View"->"Templates");**

### Hints for updating ###

We suggest the following steps in order to avoid problems while updating:

  * close Delphi
  * replace all bpl/dcp-files with the new versions
  * open Delphi

### What is TUOScript and how can i use it? ###

The files TUOScript.[bpl|dcp] contains the definition and implementation of the TUOScriptEngine. It registers the new script-language "TUOScript" like you can see it in the shipped live-templates.

To use the script-engine do the following example-steps:
1.Create a new package.Add "TUOScript100" to then requires-clause.

2.Add a new unit to the package with this example-code:
```
unit uMyScript;

interface

uses
  Classes, SysUtils, ToolsAPI, TUOScript, Dialogs;

type  
  TMyScript = class(TInterfacedObject, ITUOScriptFunctionHandler)
  private
    function DoSomething(Params : TTUOScriptParams; const AEditor : IOTAEditor) : TTUOScriptFunctionResult;
  public
    function GetFunctionCount : Integer;
    function GetFunctionName(AIndex : Integer) : WideString;
    function GetFunctionItself(AIndex : Integer) : TTUOSctiptFunction2;
  end;


implementation

var
  MyScript : TMyScript;

function TMyScript.GetFunctionCount: Integer;
begin
  Result:=1;
end;

function TMyScript.GetFunctionItself(AIndex: Integer): TTUOSctiptFunction2;
begin
  case AIndex of
    0: Result:=InsertCompo;
  end;
end;

function TMyScript.GetFunctionName(AIndex: Integer): WideString;
begin
  case AIndex of
    0: Result:='InsertComponent';
  end;
end;

function TMyScript.DoSomething(Params: TTUOScriptParams; const AEditor : IOTAEditor): TTUOScriptFunctionResult;
begin
  //write your code here
  MessageDlg('Hello Delphi', mtError, [mbOK], 0)
end;

initialization
  MyScript:=TMyScript.Create;
  TUOScriptEngine.RegisterFunctionHandler(MyScript);

finalization
  TUOScriptEngine.UnregisterFunctionHandler(MyScript);

end.
```
Now, install the package.

3.Create a live-template with the following script-part:
```
<script language="TUOScript" onenter="false" onleave="true">
  Version=1,0
  DoSomething(|Pointname1|,"Hello world")
</script>
```
Follow this rules:

  * not casesensitive
  * one command per line
  * at least one versioninfo (first is used); currently: Version=1,0
  * dont forget the script-language "TUOScript" :)
  * parameters with spaces has to be written inside ""
> > (this doses not matter for point-names)

Point-names may be used as parameters if they begin and end with the declared "delimiter" (xml-property of the code-tag).