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
			BARCODE_TYPE_UPC_A			=	0x00000001,
		  	BARCODE_TYPE_EAN_13			=	0x00000002,	
			BARCODE_TYPE_EAN_8			=	0x00000004,
			BARCODE_TYPE_UPC_E			=	0x00000008,
		 	BARCODE_TYPE_CODE_39		=	0x00000010,
			BARCODE_TYPE_CODE_32		=	0x00000020,
			BARCODE_TYPE_CODE_128		=	0x00000040,
			BARCODE_TYPE_INTERLEAVE_25 	= 	0x00000080,
			BARCODE_TYPE_INDUSTRIAL_25 	= 	0x00000100,
			BARCODE_TYPE_MATRIX_25		=	0x00000200,
			BARCODE_TYPE_CODABAR		=	0x00000400,
			BARCODE_TYPE_CODE_93		=	0x00000800,
			BARCODE_TYPE_CODE_11		=	0x00001000,
			BARCODE_TYPE_CHINA_POSTAGE 	=	0x00002000,
			BARCODE_TYPE_MSI			=	0x00004000,
			BARCODE_TYPE_BC412			=	0x00008000,
			BARCODE_TYPE_CODE_2_OF_6	=	0x00010000,
			BARCODE_TYPE_RESERVE_4_CODE	= 	0x00020000,
			BARCODE_TYPE_VP_25			=	0x00040000,
			BARCODE_TYPE_TELEPEN		=	0x00080000,
			BARCODE_TYPE_ALL			=	0x000FFFFF,
			BARCODE_TYPE_CODE_3OF9      = 	BARCODE_TYPE_CODE_39 | BARCODE_TYPE_CODE_32
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
		
		public static bool SetConfig(BarcodeType barcode_type, BarcodeCase barcode_case)
		{
			return WMS_SetConfig((ulong)barcode_type, (byte)barcode_case);
		}
		
		public static bool SetConfig(ulong barcode_type, byte barcode_case)
		{
			return WMS_SetConfig(barcode_type, barcode_case);
		}
		
		#endregion
	}
}
