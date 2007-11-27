/*
 * Erstellt mit SharpDevelop.
 * Benutzer: MaWarm
 * Datum: 27.11.2007
 * Zeit: 08:51
 * 
 * Sie können diese Vorlage unter Extras > Optionen > Codeerstellung > Standardheader ändern.
 */
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace TUO.CompactFramework2.Devices.Barcode.Pretec
{
	/// <summary>
	/// Description of MyClass.
	/// </summary>
	public class Scanner
	{
		#region TypeDefs
		[Flags]
		public enum BarcodeType : ulong
		{
			BARCODE_TYPE_UPC_A			=	0x0001,
		  	BARCODE_TYPE_EAN_13			=	0x0002,	
			BARCODE_TYPE_EAN_8			=	0x0004,
			BARCODE_TYPE_UPC_E			=	0x0008,
		 	BARCODE_TYPE_CODE_39		=	0x0010,
			BARCODE_TYPE_CODE_32		=	0x0020,
			BARCODE_TYPE_CODE_128		=	0x0040,
			BARCODE_TYPE_INTERLEAVE_25 	= 	0x0080,
			BARCODE_TYPE_INDUSTRIAL_25 	= 	0x0100,
			BARCODE_TYPE_MATRIX_25		=	0x0200,
			BARCODE_TYPE_CODABAR		=	0x0400,
			BARCODE_TYPE_CODE_93		=	0x0800,
			BARCODE_TYPE_CODE_11		=	0x1000,
			BARCODE_TYPE_CHINA_POSTAGE 	=	0x2000,
			BARCODE_TYPE_MSI			=	0x4000,
			BARCODE_TYPE_BC412			=	0x8000,
			BARCODE_TYPE_CODE_2_OF_6	=	0x00010000,
			BARCODE_TYPE_RESERVE_4_CODE	= 	0x00020000,
			BARCODE_TYPE_VP_25			=	0x00040000,
			BARCODE_TYPE_TELEPEN		=	0x00080000,
			BARCODE_TYPE_ALL			=	0x000FFFFF
		}
		
		[Flags]
		public enum BarcodeCase : byte
		{
			BARCODE_CASE_NOT_CHANGED	=	0x00,
			BARCODE_CASE_UPPER_CASE		=	0x01,
			BARCODE_CASE_LOWER_CASE		=	0x02
		}
		
			
		#endregion
		
		#region P/Invoke
		[DllImport("BarcodeDll.dll", EntryPoint="WMS_Open", SetLastError=true)]
		private static extern bool WMS_Open( 
			string portname, 
			UInt32 baudrate); 
		
		[DllImport("BarcodeDll.dll", EntryPoint="WMS_SetConfig", SetLastError=true)]
		private static extern bool WMS_SetConfig( 
			ulong barcode_type, 
			byte barcode_case);
		
		[DllImport("BarcodeDll.dll", EntryPoint="WMS_Close", SetLastError=true)]
		private static extern bool WMS_Close(); 
		
		[DllImport("BarcodeDll.dll", EntryPoint="WMS_Scan", SetLastError=true)]
		private static extern bool WMS_Scan( 
		    byte[] data,
			ref int length); 		
		#endregion
		
		#region public functions
		public static bool Open()
		{
			return Open("", 9600);
		}
		
		public static bool Open(string portname, UInt32 baudrate)
		{
			return WMS_Open(portname, baudrate);
		}
		
		public static bool Close()
		{
			return WMS_Close();
		}
		
		public static bool Scan(byte[] data, ref int length)
		{
			return WMS_Scan(data, ref length);
		}
		
		#endregion
	}
}
