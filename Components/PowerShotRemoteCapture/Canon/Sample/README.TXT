========================================================================
       PowerShot  RemoteCapurte SDK Sample Code : RelCtrl
========================================================================

NOTE:

1. You need the CDSDK for building and executing this sample application.

2. You can't use and copy all the source codes in this JpegLib folder  for developing applications.

-------------------------------------------------------
 How to execute this sample application
-------------------------------------------------------

1. Copy this RelCtrl folder and JpegLib folder to HDD on your computer.

2. Open the JpegLib.dsw in the JpegLib folder by Visual C++ 6.0, and build it.
	JpegLib.lib which is used for buliding the RelCtrl is copied automatically in the 
	RelCtrl folder.

3. Next, open the RelCtrl.dsw in the RelCtrl folder using Visual C++ 6.0, and bulid it.
    Set the path to the header files of the PS-ReC SDK and the CDSDK, 
    and the path to the libraries of the PS-ReC SDK and the CDSDK:
    The default settings are;
	C:\PSReCSDK\inc and C:\PSReCSDK\lib for PowerShot  RemoteCapurte SDK.
	C:\CDSDK\inc and C:\CDSDK\lib for CDSDK.

      If you want to charnge the paths, change them.

4. Copy all the files in the \PSReCSDK\Redist folder and \CDSDK\Redist folder to the \Debug 
    or \Release folder.

-------------------------------------------------------
 How to use of this sample application
-------------------------------------------------------

1. Connect the camera with PC beforehand.
2. Execute sample application, then dialog will appear.
3. Push the "Connect" button to establish the connection to camera.
4-a. Push the "Release" button to take a picture.
4-b. Push the "Start" button to start the view finder function.
     NOTE: Not all camera models support the view finder function.
     Push the "Stop" button to finish the view finder function.
5. Press "Disconnect" button to finish the connection.


