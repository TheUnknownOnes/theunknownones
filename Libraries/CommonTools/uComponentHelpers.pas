unit uComponentHelpers;

interface

uses
  Windows, VirtualTrees;

{$REGION 'VirtualStringTree Helpers'}
procedure VST_ScanEditorKeys(var AVKey: Word; ATree: TVirtualStringTree);
{$ENDREGION}

implementation

uses
  SndKey32, Dialogs, SysUtils;

{$REGION 'VirtualStringTree Helpers'}
procedure VST_ScanEditorKeys(var AVKey: Word; ATree: TVirtualStringTree);
//Beim Tastendruck auf einem VST wird automatisch ein Editor geöffnet
// Aufruf im OnKeyDown Event
var
  InputStr : String;

  {$REGION 'VirtualKey in Char übersetzen'}
 
  function GetCharFromVirtualKey(Key: Word): string;
  var
     keyboardState: TKeyboardState;
     asciiResult: Integer;
  begin
     GetKeyboardState(keyboardState) ;

     SetLength(Result, 2) ;
     asciiResult := ToAscii(key, MapVirtualKey(key, 0), keyboardState, @Result[1], 0) ;
     case asciiResult of
       0: Result := '';
       1: SetLength(Result, 1) ;
       2:;
       else
         Result := '';
     end;
  end;
  {$ENDREGION}
begin
  if not ATree.IsEditing then
  begin
    if AVKey=VK_RETURN then
    begin
      AVKey:=0;
      ATree.EditNode(ATree.FocusedNode, ATree.FocusedColumn);
    end
    else
    begin
      InputStr:=GetCharFromVirtualKey(AVKey);
      If Length(InputStr)>0 then
        if Copy(InputStr,1,1)[1]>=#32 then
        begin
          if ATree.EditNode(ATree.FocusedNode, ATree.FocusedColumn) and
             (Length(trim(InputStr))>0) then
            SendKeys(PChar(Copy(InputStr,1,1)), True);

          AVKey:=0;
        end;
    end;
  end;
end;
{$ENDREGION}

end.
