### What is KeyboardLED? ###

KeyboardLED is a unit for Delphi [6|7|2005] enabling you to set the state of the three keyboard LEDs.
This is done by direct access to the keyboard buffers and not by virtually pressing any key.
### Disclaimer ###

NO WARRANTY
THE SOFTWARE INCLUDED IN THIS DISTRIBUTION IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OF THE SOFTWARE BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. YOUR USE OF THE SOFTWARE SIGNIFIES THAT YOU HAVE READ, UNDERSTOOD AND AGREE TO THESE TERMS AND CONDITIONS.
### Datatypes and Routines ###

TKeyboardLEDState = (klsScrollLock, klsNUMLock, klsCAPSLock);
TKeyboardLEDStates = set of TKeyboardLEDState;

function SetKeyboardLED(aKeyLEDState : TKeyboardLEDStates): Byte;
function ResetKeyboardLED: Byte; //sets the LEDs back to normal
### Known Issues... ###

  * This is a just-for-fun-library. So please read the disclaimer.
  * Since Windows NT, 2k and above does not allow any software to access interrupts directly (except for those running in kernel mode) these routines may interfere with Windows system routines and could lead to unwanted symptoms. (e.g. 'swallowed' characters) So far we encountered this behaviour only when changing LED states very quickly (<15ms)
  * This routines will not work with older XT keyboard controllers
  * They may not work with some of the newer adapters (USB is still untested)