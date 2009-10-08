********************************************************
*                     KeyboardLED                      *
*                                                      *
*  Routines for switching the KeyboardLEDs on and off  *
*                                                      *
********************************************************

NO WARRANTY
THE SOFTWARE INCLUDED IN THIS DISTRIBUTION IS PROVIDED "AS IS", WITHOUT
WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OF THE SOFTWARE OR LIBRA
COMPUTER SYSTEMS LTD. BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE. YOUR USE OF THE SOFTWARE SIGNIFIES THAT YOU
HAVE READ, UNDERSTOOD AND AGREE TO THESE TERMS AND CONDITIONS.

Installation
============

Simply copy the file KeyboardLED_Dxx.dcu to a directory in your Delphi search 
path. Be sure to choose the DCU file that is compiled for your compiler version.

Usage
=====

- Add the KeyboardLED_Dxx unit to the the uses clause of your project.
- Be sure to copy the file INPOUT32.dll from one of the DEMO folders to a folder 
  in your search path (preferrably Windows\System32)
- Now you are ready to use the contained routines.

DataTypes
=========

  TKeyboardLEDState = (klsScrollLock,     //ScrollLock LED is on
                      klsNUMLock,        //NUMLock LED is on
                      klsCAPSLock);      //CAPSLock LED is on

  TKeyboardLEDStates= set of TKeyboardLEDState;
  
Functions
=========

  function SetKeyboardLED(aKeyLEDState : TKeyboardLEDStates): Byte;
  function ResetKeyboardLED: Byte; //sets the LEDs back to normal
  
Example
=======
  procedure TForm1.Button1Click(Sender: TObject);
  var
    LEDState: TKeyboardLEDStates;
  begin
    LEDState:=[];

    if CheckBox1.Checked then
      LEDState:=LEDState + [klsScrollLock];
  
    if CheckBox2.Checked then
      LEDState:=LEDState + [klsCAPSLock];
  
    if CheckBox3.Checked then
      LEDState:=LEDState + [klsNUMLock];
  
    SetKeyboardLED(LEDState);
  end;








