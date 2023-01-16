{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2023 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnTwain; 
{* |<PRE>
================================================================================
* 软件名称：外接设备组件包
* 单元名称：实现扫描仪图象采集单元
* 单元作者：rarnu(rarnu@cnpack.org)
* 备    注：爱普生扫描仪可用，使用EPSON V200 API，本单元仅是略做封装
* 开发平台：Windows2003 Server + Delphi2007 up2
* 兼容测试：Windows2000/XP/2003/Vista + Delphi 7/2006/2007/2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2008.08.14 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics;

const
  APP_PROTOCOLMAJOR = 4;
  APP_PROTOCOLMINOR = 0;
  VALID_HANDLE = 32;

const
  TWON_PROTOCOLMAJOR = 1;
  TWON_PROTOCOLMINOR = 7;
  TWON_ARRAY = 3;
  TWON_ENUMERATION = 4;
  TWON_ONEVALUE = 5;
  TWON_RANGE = 6;
  TWON_ICONID = 962;
  TWON_DSMID = 461;
  TWON_DSMCODEID = 63;
  TWON_DONTCARE8 = $FF;
  TWON_DONTCARE16 = $FFFF;
  TWON_DONTCARE32 = $FFFFFFFF;
  TWCY_USA = 1;
  TWCY_CANADA = 2;
  TWCY_MEXICO = 3;
  TWCY_BRITAIN = 6;
  TWCY_FRANCE = 33;
  TWCY_JAPAN = 81;
  TWCY_KOREA = 82;
  TWCY_CHINA = 86;
  TWCY_HONGKONG = 852;
  TWCY_TAIWAN = 886;
  TWLG_DAN = 0;   //Danish
  TWLG_DUT = 1;   //Dutch
  TWLG_ENG = 2;   //International English
  TWLG_FCF = 3;   //French Canadian
  TWLG_FIN = 4;   //Finnish
  TWLG_FRN = 5;   //French
  TWLG_GER = 6;   //German
  TWLG_ICE = 7;   //Icelandic
  TWLG_ITN = 8;   //Italian
  TWLG_NOR = 9;   //Norwegian
  TWLG_POR = 10;  //Portuguese
  TWLG_SPA = 11;  //Spanish
  TWLG_SWE = 12;  //Swedish
  TWLG_USA = 13;  //U.S. English
  TWLG_USERLOCALE = $FFFF;
  TWLG_CHINESE = 37;
  TWLG_CHINESE_PRC = 39;  // People's Republic of China
  TWLG_CHINESE_SIMPLIFIED = 41;
  TWLG_CHINESE_TAIWAN = 42;
  TWLG_CHINESE_TRADITIONAL = 43;
  TWTY_INT8 = $0000;      // Means Item is a TW_INT8
  TWTY_INT16 = $0001;      // Means Item is a TW_INT16
  TWTY_INT32 = $0002;      // Means Item is a TW_INT32
  TWTY_UINT8 = $0003;      // Means Item is a TW_UINT8
  TWTY_UINT16 = $0004;      // Means Item is a TW_UINT16
  TWTY_UINT32 = $0005;      // Means Item is a TW_UINT32
  TWTY_BOOL = $0006;      // Means Item is a TW_BOOL
  TWTY_FIX32 = $0007;      // Means Item is a TW_FIX32
  TWTY_FRAME = $0008;      // Means Item is a TW_FRAME
  TWTY_STR32 = $0009;      // Means Item is a TW_STR32
  TWTY_STR64 = $000a;      // Means Item is a TW_STR64
  TWTY_STR128 = $000b;      // Means Item is a TW_STR128
  TWTY_STR255 = $000c;      // Means Item is a TW_STR255
  DG_CONTROL = $00000001;  // data pertaining to control
  DG_IMAGE = $00000002;  // data pertaining to raster images
  DG_IMAGE_OR_CONTROL = $00000003;   //is " DG_CONTROL|DG_IMAGE " in C++
  DAT_NULL = $0000;  // No data or structure.
  DAT_CUSTOMBASE = $8000;  // Base of custom DATs.
          // Data Argument Types for the DG_CONTROL Data Group.
  DAT_CAPABILITY = $0001;  // TW_CAPABILITY
  DAT_EVENT = $0002;  // TW_EVENT
  DAT_IDENTITY = $0003;  // TW_IDENTITY
  DAT_PARENT = $0004;  // TW_HANDLE, app win handle in Windows
  DAT_PENDINGXFERS = $0005;  // TW_PENDINGXFERS
  DAT_SETUPMEMXFER = $0006;  // TW_SETUPMEMXFER
  DAT_SETUPFILEXFER = $0007;  // TW_SETUPFILEXFER
  DAT_STATUS = $0008;  // TW_STATUS
  DAT_USERINTERFACE = $0009;  // TW_USERINTERFACE
  DAT_XFERGROUP = $000a;  // TW_UINT32
  DAT_TWUNKIDENTITY = $000b;  // TW_TWUNKIDENTITY
          // Data Argument Types for the DG_IMAGE Data Group.
  DAT_IMAGEINFO = $0101;    // TW_IMAGEINFO
  DAT_IMAGELAYOUT = $0102;    //TW_IMAGELAYOUT
  DAT_IMAGEMEMXFER = $0103;    //TW_IMAGEMEMXFER
  DAT_IMAGENATIVEXFER = $0104;    //TW_UINT32 loword is hDIB, PICHandle
  DAT_IMAGEFILEXFER = $0105;    //Null data
  DAT_CIECOLOR = $0106;    //TW_CIECOLOR
  DAT_GRAYRESPONSE = $0107;    //TW_GRAYRESPONSE
  DAT_RGBRESPONSE = $0108;    //TW_RGBRESPONSE
  DAT_JPEGCOMPRESSION = $0109;    //TW_JPEGCOMPRESSION
  DAT_PALETTE8 = $010A;    //TW_PALETTE8

  // All message constants are unique.
  MSG_NULL = $0000;   // Used in TW_EVENT structure
  MSG_CUSTOMBASE = $8000;   // Base of custom messages
     // Generic messages may be used with any of several DATs.
  MSG_GET = $0001;   // Get one or more values
  MSG_GETCURRENT = $0002;   // Get current value
  MSG_GETDEFAULT = $0003;   // Get default (e.g. power up) value
  MSG_GETFIRST = $0004;   // Get first of a series of items, e.g. DSs
  MSG_GETNEXT = $0005;   // Iterate through a series of items.
  MSG_SET = $0006;   // Set one or more values
  MSG_RESET = $0007;   // Set current value to default value
     // Messages used with DAT_NULL
  MSG_XFERREADY = $0101;   // The data source has data ready
  MSG_CLOSEDSREQ = $0102;   // Request for App. to close DS
  MSG_CLOSEDSOK = $0103;   // Tell the Application. to save the state.
     // Messages used with a pointer to a DAT_STATUS structure
  MSG_CHECKSTATUS = $0201;   // Get status information
     // Messages used with a pointer to DAT_PARENT data
  MSG_OPENDSM = $0301;   // Open the DSM
  MSG_CLOSEDSM = $0302;   // Close the DSM
     // Messages used with a pointer to a DAT_IDENTITY structure
  MSG_OPENDS = $0401;   // Open a data source
  MSG_CLOSEDS = $0402;   // Close a data source
  MSG_USERSELECT = $0403;   // Put up a dialog of all DS
    // Messages used with a pointer to a DAT_USERINTERFACE structure
  MSG_DISABLEDS = $0501;   // Disable data transfer in the DS
  MSG_ENABLEDS = $0502;   // Enable data transfer in the DS
    // Messages used with a pointer to a DAT_EVENT structure
  MSG_PROCESSEVENT = $0601; 
    // Messages used with a pointer to a DAT_PENDINGXFERS structure
  MSG_ENDXFER = $0701; 

  // Capabilities

  CAP_CUSTOMBASE = $8000;   //Base of custom capabilities
   // all data sources are REQUIRED to support these caps
  CAP_XFERCOUNT = $0001; 
   // image data sources are REQUIRED to support these caps
  ICAP_COMPRESSION = $0100;
  ICAP_PIXELTYPE = $0101;
  ICAP_UNITS = $0102;   //default is TWUN_INCHES
  ICAP_XFERMECH = $0103; 
   // all data sources MAY support these caps
  CAP_AUTHOR = $1000;
  CAP_CAPTION = $1001;
  CAP_FEEDERENABLED = $1002;
  CAP_FEEDERLOADED = $1003;
  CAP_TIMEDATE = $1004;
  CAP_SUPPORTEDCAPS = $1005;
  CAP_EXTENDEDCAPS = $1006;
  CAP_AUTOFEED = $1007;
  CAP_CLEARPAGE = $1008;
  CAP_FEEDPAGE = $1009;
  CAP_REWINDPAGE = $100a;
  CAP_INDICATORS = $100b;    //Added 1.1
  CAP_SUPPORTEDCAPSEXT = $100c; 	// Added 1.6
  CAP_PAPERDETECTABLE = $100d; 	// Added 1.6
  CAP_UICONTROLLABLE = $100e; 	// Added 1.6
   // image data sources MAY support these caps
  ICAP_AUTOBRIGHT = $1100;
  ICAP_BRIGHTNESS = $1101;
  ICAP_CONTRAST = $1103;
  ICAP_CUSTHALFTONE = $1104;
  ICAP_EXPOSURETIME = $1105;
  ICAP_FILTER = $1106;
  ICAP_FLASHUSED = $1107;
  ICAP_GAMMA = $1108;
  ICAP_HALFTONES = $1109;
  ICAP_HIGHLIGHT = $110a;
  ICAP_IMAGEFILEFORMAT = $110c;
  ICAP_LAMPSTATE = $110d;
  ICAP_LIGHTSOURCE = $110e;
  ICAP_ORIENTATION = $1110;
  ICAP_PHYSICALWIDTH = $1111;
  ICAP_PHYSICALHEIGHT = $1112;
  ICAP_SHADOW = $1113;
  ICAP_FRAMES = $1114;
  ICAP_XNATIVERESOLUTION = $1116;
  ICAP_YNATIVERESOLUTION = $1117;
  ICAP_XRESOLUTION = $1118;
  ICAP_YRESOLUTION = $1119;
  ICAP_MAXFRAMES = $111a;
  ICAP_TILES = $111b;
  ICAP_BITORDER = $111c;
  ICAP_CCITTKFACTOR = $111d;
  ICAP_LIGHTPATH = $111e;
  ICAP_PIXELFLAVOR = $111f;
  ICAP_PLANARCHUNKY = $1120;
  ICAP_ROTATION = $1121;
  ICAP_SUPPORTEDSIZES = $1122;
  ICAP_THRESHOLD = $1123;
  ICAP_XSCALING = $1124;
  ICAP_YSCALING = $1125;
  ICAP_BITORDERCODES = $1126;
  ICAP_PIXELFLAVORCODES = $1127;
  ICAP_JPEGPIXELTYPE = $1128;
  ICAP_TIMEFILL = $112a;
  ICAP_BITDEPTH = $112b;
  ICAP_BITDEPTHREDUCTION = $112c;             //Added 1.5

   //Return Codes and Condition Codes section
   // Return Codes: DSM_Entry and DS_Entry may return any one of these values.
  TWRC_CUSTOMBASE = $8000;
  TWRC_SUCCESS = 0;
  TWRC_FAILURE = 1;   //App may get TW_STATUS for info on failure
  TWRC_CHECKSTATUS = 2;   //"tried hard"; get status
  TWRC_CANCEL = 3;
  TWRC_DSEVENT = 4;
  TWRC_NOTDSEVENT = 5;
  TWRC_XFERDONE = 6;
  TWRC_ENDOFLIST = 7;   //After MSG_GETNEXT if nothing left
    //Condition Codes: App gets these by doing DG_CONTROL DAT_STATUS MSG_GET.
  TWCC_CUSTOMBASE = $8000;
  TWCC_SUCCESS = 0;   //It worked!
  TWCC_BUMMER = 1;   //Failure due to unknown causes
  TWCC_LOWMEMORY = 2;   //Not enough memory to perform operation
  TWCC_NODS = 3;   //No Data Source
  TWCC_MAXCONNECTIONS = 4;   //DS is connected to max possible apps
  TWCC_OPERATIONERROR = 5;   //DS or DSM reported error, app shouldn't
  TWCC_BADCAP = 6;   //Unknown capability
  TWCC_BADPROTOCOL = 9;   //Unrecognized MSG DG DAT combination
  TWCC_BADVALUE = 10;   //Data parameter out of range
  TWCC_SEQERROR = 11;   //DG DAT MSG out of expected sequence
  TWCC_BADDEST = 12;   //Unknown destination App/Src in DSM_Entry

  // ICAP_UNITS values (UN_ means UNits)
  TWUN_INCHES = 0;
  TWUN_CENTIMETERS = 1;
  TWUN_PICAS = 2;
  TWUN_POINTS = 3;
  TWUN_TWIPS = 4;
  TWUN_PIXELS = 5; 

  // ICAP_PIXELTYPE values (PT_ means Pixel Type)
  TWPT_BW = 0;
  TWPT_GRAY = 1;
  TWPT_RGB = 2;
  TWPT_PALETTE = 3;
  TWPT_CMY = 4;
  TWPT_CMYK = 5;
  TWPT_YUV = 6;
  TWPT_YUVK = 7;
  TWPT_CIEXYZ = 8; 

  // Flags used in TW_MEMORY structure.
  TWMF_APPOWNS = $01;
  TWMF_DSMOWNS = $02;
  TWMF_DSOWNS = $04;
  TWMF_POINTER = $08;
  TWMF_HANDLE = $10; 

  // ICAP_PIXELFLAVOR values (PF_ means Pixel Flavor)
  TWPF_CHOCOLATE = 0;    // zero pixel represents darkest shade
  TWPF_VANILLA = 1;    // zero pixel represents lightest shade

  // ICAP_IMAGEFILEFORMAT values (FF_means File Format)
  TWFF_TIFF = 0;    // Tagged Image File Format
  TWFF_PICT = 1;    // Macintosh PICT
  TWFF_BMP = 2;    // Windows Bitmap
  TWFF_XBM = 3;    // X-Windows Bitmap
  TWFF_JFIF = 4;    // JPEG File Interchange Format
  TWFF_FPX = 5;    // Flash Pix
  TWFF_TIFFMULTI = 6;    // Multi-page tiff file
  TWFF_PNG = 7;
  TWFF_SPIFF = 8;
  TWFF_EXIF = 9;
  TWSX_NATIVE = 0;
  TWSX_FILE = 1;
  TWSX_MEMORY = 2;

type
  TW_HANDLE = Word;

  TW_MEMREF = pointer; 

// TW_HUGE   = Longint;
  TW_STR32 = array[0..33] of Char; 
// TW_STR64  = Array [0..65] of Char;
// TW_STR128 = Array [0..129] of Char;
// TW_STR255 = Array [0..255] of Char;
//  TW_INT8   = ShortInt;

  TW_INT16 = Smallint;

  TW_INT32 = Longint;

  TW_UINT8 = Byte;

  TW_UINT16 = Word;   // Unsinged  integer  !!!

  TW_UINT32 = Longword;

  TW_BOOL = Word;   // Unsinged  Short Boolean  !!!

  pTW_UINT16 = ^TW_UINT16;

  TW_FIX32 = packed record // Fixed point structure type.
    Whole: TW_INT16;         // maintains the sign
    Frac: TW_UINT16;
  end;

  pTW_FIX32 = ^TW_FIX32;

  TW_VERSION = packed record
    MajorNum: TW_UINT16;   // Major revision number of the software.
    MinorNum: TW_UINT16;   // Incremental revision number of the software.
    Language: TW_UINT16;   // e.g. TWLG_SWISSFRENCH
    Country: TW_UINT16;   // e.g. TWCY_SWITZERLAND
    Info: TW_STR32;    // e.g. "1.0b3 Beta release"
  end;

  TW_IDENTITY = packed record
    Id: TW_UINT32;   // Unique number.  In Windows, app hWnd
    Version: TW_VERSION;  // Identifies the piece of code
    ProtocolMajor: TW_UINT16;   // App and DS must set to TWON_PROTOCOLMAJOR
    ProtocolMinor: TW_UINT16;   // App and DS must set to TWON_PROTOCOLMINOR
    SupportedGroups: TW_UINT32;   // Bit field OR combination of DG_ constants
    Manufacturer: TW_STR32;    // Manufacturer name, e.g. "Hewlett-Packard"
    ProductFamily: TW_STR32;    // Product family name, e.g. "ScanJet"
    ProductName: TW_STR32;    // Product name, e.g. "ScanJet Plus"
  end;

  pTW_IDENTITY = ^TW_IDENTITY;

  TW_IMAGEINFO = packed record // DAT_IMAGEINFO. App gets detailed image info from DS with this.
    XResolution: TW_FIX32;         // Resolution in the horizontal
    YResolution: TW_FIX32;         // Resolution in the vertical
    ImageWidth: TW_INT32;         // Columns in the image, -1 if unknown by DS
    ImageLength: TW_INT32;         // Rows in the image, -1 if unknown by DS
    SamplesPerPixel: TW_INT16;         // Number of samples per pixel, 3 for RGB
    BitsPerSample: array[0..7] of TW_INT16;     // Number of bits for each sample
    BitsPerPixel: TW_INT16;         // Number of bits for each padded pixel
    Planar: TW_BOOL;          // True if Planar, False if chunky
    PixelType: TW_INT16;         // How to interp data; photo interp (TWPT_)
    Compression: TW_UINT16;        // How the data is compressed (TWCP_xxxx)
  end;

  pTW_IMAGEINFO = ^TW_IMAGEINFO;

  TW_ONEVALUE = packed record
    ItemType: TW_UINT16;
    Item: TW_UINT32;
  end;

  pTW_ONEVALUE = ^TW_ONEVALUE;

  TW_CAPABILITY = packed record //DAT_CAPABILITY. Used by app to get/set capability from/in a data source.
    Cap, ConType: TW_UINT16;
    hContainer: THandle;
  end;

  pTW_CAPABILITY = ^TW_CAPABILITY;

  TW_SETUPMEMXFER = packed record
    MinBufSize: TW_UINT32;
    MaxBufSize: TW_UINT32;
    Preferred: TW_UINT32;
  end;

  pTW_SETUPMEMXFER = ^TW_SETUPMEMXFER;

  TW_USERINTERFACE = packed record
    ShowUI: TW_BOOL;   // TRUE if DS should bring up its UI
    ModalUI: TW_BOOL;  // For Mac only - true if the DS's UI is modal
    hParent: TW_HANDLE;  // For windows only - App window handle
  end;

  pTW_USERINTERFACE = ^TW_USERINTERFACE;

  TW_EVENT = packed record
    pEvent: TW_MEMREF;     // Windows pMSG or Mac pEvent.
    TWMessage: TW_UINT16;     // TW msg from data source, e.g. MSG_XFERREADY
  end;

  pTW_EVENT = ^TW_EVENT;

  TW_PENDINGXFERS = packed record
    Count: TW_UINT16;     // Number of additional "images" pending.
    Reserved: TW_UINT32;
  end;

  pTW_PENDINGXFERS = ^TW_PENDINGXFERS;

  TW_ELEMENT8 = packed record
    Index: TW_UINT8;           // Value used to index into the color table.
    Channel1: TW_UINT8;        // First  tri-stimulus value (e.g Red)
    Channel2: TW_UINT8;        // Second tri-stimulus value (e.g Green)
    Channel3: TW_UINT8;        // Third  tri-stimulus value (e.g Blue)
  end;

  pTW_ELEMENT8 = ^TW_ELEMENT8;

  TW_PALETTE8 = packed record
    NumColors: TW_UINT16;      // Number of colors in the color table.
    PaletteType: TW_UINT16;    // TWPA_xxxx, specifies type of palette.
    Colors: array[0..255] of TW_ELEMENT8;   // TWPA_xxxx, specifies type of palette.
  end;

  pTW_PALETTE8 = ^TW_PALETTE8;

  TW_MEMORY = packed record
    Flags: TW_UINT32;           // Any combination of the TWMF_ constants.
    Length: TW_UINT32;          // Number of bytes stored in buffer TheMem.
    TheMem: TW_MEMREF;          // Pointer or handle to the allocated memory buffer.
  end;

  pTW_MEMORY = ^TW_MEMORY;

  TW_IMAGEMEMXFER = packed record
    Compression: TW_UINT16;    // How the data is compressed
    BytesPerRow: TW_UINT32;    // Number of bytes in a row of data
    Columns: TW_UINT32;        // How many columns
    Rows: TW_UINT32;           // How many rows
    XOffset: TW_UINT32;        // How far from the side of the image
    YOffset: TW_UINT32;        // How far from the top of the image
    BytesWritten: TW_UINT32;   // How many bytes written in Memory
    Memory: TW_MEMORY;         // Mem struct used to pass actual image data
  end;

  pTW_IMAGEMEMXFER = ^TW_IMAGEMEMXFER;

  TW_SETUPFILEXFER = packed record
    FileName: array[0..255] of Char;
    Format: TW_UINT16;             // Any TWFF_ constant
    VRefNum: TW_INT16;             // Used for Mac only
  end;

  pTW_SETUPFILEXFER = ^TW_SETUPFILEXFER;

  TW_ENUMERATION = packed record
    ItemType: TW_UINT16;
    NumItems: TW_UINT32;        // How many items in ItemList
    CurrentIndex: TW_UINT32;    // Current value is in ItemList[CurrentIndex]
    DefaultIndex: TW_UINT32;    // Powerup value is in ItemList[DefaultIndex]
    ItemList: array[0..0] of TW_UINT8;  // Array of ItemType values starts here
  end;

  pTW_ENUMERATION = ^TW_ENUMERATION;

  DSM_Entry = function(pOrigin: pTW_IDENTITY; pDest: pTW_IDENTITY; DG: TW_UINT32; DAT: TW_UINT16; MSG: TW_UINT16; pData: TW_MEMREF): TW_UINT16; stdcall;

  TOnTwMessage = procedure(Sender: TObject; Msg: string) of object;

  TOnCapture = procedure(Sender: TObject; bmp: TBitmap) of object;

  TOnFileNameNeeded = procedure(Sender: TObject; var FileName: string) of object;

  TtransferType = (doNativeTransfer, doFileTransfer, doMemTransfer);

  TCnTwain = class(TComponent)
  private
    FAppID: TW_IDENTITY;
    FdsID: TW_IDENTITY;
    twUI: TW_USERINTERFACE;
    FHandle: HWND;
    FIsDSMOpen: Boolean;
    FIsDSOpen: Boolean;
    FIsDSEnabled: Boolean;
    FTransferType: TTransferType;
    hDSMDLL: THandle;
    lpDSM_Entry: DSM_Entry;
    OldWndProc: TFarProc;
    NewWndProc: Pointer;
    FHooked: Boolean;
    FAutoFeed: Boolean;
    FOnTwMessage: TOnTwMessage;
    FOnCapture: TOnCapture;
    FOnFileNameNeeded: TOnFileNameNeeded;
    procedure HookWin;
    procedure UnHookWin;
  protected
    function SelectDS: TW_UINT16;
    procedure WndProc(var Message: TMessage);
    function ProcessTWMessage(var aMsg: TMessage; TwhWnd: THandle): Boolean;
    function OpenDSM: TW_UINT16;        //  DSM
    function CloseDSM: TW_UINT16;
    function OpenDS: TW_UINT16;         //  DS
    function CloseDS: TW_UINT16;
    function XferMechDS: TW_UINT16;
    function AutoFeedDS: TW_UINT16;
    function EnableDS(Show: Boolean): TW_UINT16;   // UI
    function DisableDS: TW_UINT16;
    procedure TransferImage;
    procedure NativeTransfer;
    procedure FileTransfer;
    procedure MemoryTransfer;
    procedure DoXferDone(hDib: THandle);
    procedure DoTwMessage(Msg: string; TerminateDS: Boolean = True);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {* 连接并打开扫描仪 }
    function Acquire(Show: Boolean): TW_UINT16; 
    {* 获取 Device 信息}
    function GetDSInfo(var DsID: TW_IDENTITY): TW_UINT16; 
    {* 获取组件信息 }
    function GetComponentInfo(var DsID: TW_IDENTITY): TW_UINT16; 
    {* 选择源，当计算机连接多个扫描仪时有效 }
    function SelectSource: TW_UINT16; 
    {* 断开并关闭扫描仪 }
    procedure Terminate; 
    {* 判断 DSM 服务是否开启 }
    property IsDSMOpen: Boolean read FIsDSMOpen; 
    {* 判断 Device 是否开启 }
    property IsDSOpen: Boolean read FIsDSOpen; 
    {* 判断 Device 是否可用 }
    property IsDSEnabled: Boolean read FIsDSEnabled;
  published
    {* 是否自动返回 }
    property AutoFeed: Boolean read FAutoFeed write FAutoFeed; 
    {* 数据传送类型 }
    property TransferType: TtransferType read FTransferType write FTransferType; 
    {* 捕获图像时触发事件 }
    property OnCaptrue: TOnCapture read FOnCapture write FOnCapture; 
    {* 捕获错误信息时触发事件 }
    property OnErrorMessage: TOnTwMessage read FOnTwMessage write FOnTwMessage; 
    {* 请求文件名时触发事件 }
    property OnFileNameNeeded: TOnFileNameNeeded read FOnFileNameNeeded write FOnFileNameNeeded;
  end;

implementation

function FIX32ToFloat(fix32: TW_FIX32): Double;
begin
  Result := fix32.Whole + (fix32.Frac / 65536.0);
end;

function DibNumColors(pv: Pointer): Word;
var
  Bits: integer;
begin
  if pBITMAPINFOHEADER(pv)^.biSize <> sizeof(BITMAPCOREHEADER) then
  begin
    if pBITMAPINFOHEADER(pv)^.biClrUsed <> 0 then
    begin
      Result := pBITMAPINFOHEADER(pv)^.biClrUsed;
      Exit;
    end;
    Bits := pBITMAPINFOHEADER(pv)^.biBitCount;
  end
  else
    Bits := pBITMAPCOREHEADER(pv)^.bcBitCount;
  case Bits of
    1:
      Result := 2;
    4:
      Result := 16;
    8:
      Result := 256;
  else
    Result := 0;
  end;
end;

function CreateBIPalette(lpbi: pBITMAPINFOHEADER): HPALETTE;
var
  pRgb: pRGBQUAD;
  nNumColors: Word;
  hPal: HGLOBAL;
  pPal: pLOGPALETTE;
  i: integer;
  Red, Green, Blue: Byte;
begin
  Result := 0;
  if lpbi = nil then
    Exit;
  if lpbi^.biSize <> sizeof(BITMAPINFOHEADER) then
    Exit;
  pRgb := pRGBQUAD(Longint(lpbi) + Word(lpbi^.biSize));
  nNumColors := DibNumColors(lpbi);
  if nNumColors <> 0 then
  begin
    hPal := GlobalAlloc(GPTR, sizeof(LOGPALETTE) + nNumColors * sizeof(PALETTEENTRY));
    pPal := GlobalLock(hPal);
    if pPal = nil then
      Exit;
    pPal^.palNumEntries := nNumColors;
    pPal^.palVersion := $0300;
    for i := 0 to nNumColors - 1 do
    begin
      pPal^.palPalEntry[i].peRed := pRGBQUAD(Longint(pRgb) + i)^.rgbRed;
      pPal^.palPalEntry[i].peGreen := pRGBQUAD(Longint(pRgb) + i)^.rgbGreen;
      pPal^.palPalEntry[i].peBlue := pRGBQUAD(Longint(pRgb) + i)^.rgbBlue;
      pPal^.palPalEntry[i].peFlags := 0;
    end;
    Result := CreatePalette(pPal^);
    GlobalUnlock(hPal);
    GlobalFree(hPal);
  end
  else if lpbi^.biBitCount = 24 then
  begin
    nNumColors := 256;
    hPal := GlobalAlloc(GPTR, sizeof(LOGPALETTE) + nNumColors * sizeof(PALETTEENTRY));
    pPal := GlobalLock(hPal);
    if pPal = nil then
      Exit;
    pPal^.palNumEntries := nNumColors;
    pPal^.palVersion := $0300;
    Red := 0;
    Green := 0;
    Blue := 0;
    for i := 0 to pPal^.palNumEntries - 1 do
    begin
      pPal^.palPalEntry[i].peRed := Red;
      pPal^.palPalEntry[i].peGreen := Green;
      pPal^.palPalEntry[i].peBlue := Blue;
      pPal^.palPalEntry[i].peFlags := 0;
      Inc(Red, 32);
      if Red = 0 then
      begin
        Inc(Green, 32);
        if Green = 0 then
          Inc(Blue, 64);
      end;
    end;
    Result := CreatePalette(pPal^);
    GlobalUnlock(hPal);
    GlobalFree(hPal);
  end;
end;

procedure FlipBitMap(hWindow, hBM: THandle; PixType: TW_INT16);
var
  pDib: pByte;
  pbmi: pBITMAPINFO;
  bmpWidth, bmpHeight, Linelength: Longint;
  indexH, items, i: integer;
  SizeImage, ClrUsed, offset: DWord;
  BitCount: Word;
  temp: THandle;
  tempptr, tempptrsave, pbuffer: pByte;
  pixels: TW_UINT16;
  SaveRed, SaveBlue: Byte;
begin
  pDib := GlobalLock(hBM);
  pbmi := pBITMAPINFO(pDib);
  bmpWidth := pbmi^.bmiHeader.biWidth;
  bmpHeight := pbmi^.bmiHeader.biHeight;
  SizeImage := pbmi^.bmiHeader.biSizeImage;
  BitCount := pbmi^.bmiHeader.biBitCount;
  ClrUsed := pbmi^.bmiHeader.biClrUsed;
  temp := GlobalAlloc(GHND, SizeImage);
  if temp <> 0 then
  begin
    tempptr := GlobalLock(temp);
    tempptrsave := tempptr; 
    // calculate offset to start of the bitmap data
    offset := Sizeof(BITMAPINFOHEADER);
    Inc(offset, ClrUsed * sizeof(RGBQUAD));
    Linelength := (((bmpWidth * BitCount + 31) div 32) * 4); 
    //Goto Last line in bitmap
    Inc(offset, Linelength * (bmpHeight - 1));
    Inc(pDib, offset);       // pDib = pDib + offset - Linelength;
    Dec(pDib, Linelength);
    for indexH := 1 to bmpHeight - 1 do
    begin
      Move(pDib^, tempptr^, Linelength);
      Dec(pDib, Linelength);
      Inc(tempptr, Linelength);
    end; 
    // Copy temp over hBM
    pbuffer := pByte(pbmi);
    Inc(pbuffer, Sizeof(BITMAPINFOHEADER));
    Inc(pbuffer, ClrUsed * Sizeof(RGBQUAD));
    Move(tempptrsave^, pbuffer^, SizeImage);  // memcpy(pbuffer, tempptrsave, SizeImage);
    if PixType = TWPT_RGB then
    begin
      pbuffer := pByte(pbmi);
      Inc(pbuffer, sizeof(BITMAPINFOHEADER));
      Inc(pbuffer, ClrUsed * sizeof(RGBQUAD));
      pixels := pbmi^.bmiHeader.biWidth;
      for items := 0 to bmpHeight - 1 do
      begin
        tempptr := pbuffer;
        for i := 0 to pixels - 1 do
        begin
          //Switch Red byte and Blue byte
          SaveRed := Byte(tempptr^);
          SaveBlue := pByte(Longword(tempptr) + 2)^;
          tempptr^ := SaveBlue;
          pByte(Longword(tempptr) + 2)^ := SaveRed;
          Inc(tempptr, 3);
        end;
        Inc(pbuffer, Linelength);
      end;
    end;
    GlobalUnlock(hBM);
    GlobalUnlock(temp);
    GlobalFree(temp);
  end
  else
  begin
    GlobalUnlock(hBM); 
    // DoTwMessage('Could not allocate enough memory to flip image', False);
  end;
end;

constructor TCnTwain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHooked := False;
  with AOwner as TWinControl do
    FHandle := Handle; 
  // Init ApplicationIdentity.
  FAppID.Id := 0;                         // Source Manager will assign real value
  with FAppID.Version do
  begin
    MajorNum := APP_PROTOCOLMAJOR;
    MinorNum := APP_PROTOCOLMINOR;
    Language := TWLG_CHINESE_SIMPLIFIED;  // TWLG_ENG;
    Country := TWCY_CHINA;
    strcopy(Info, 'RaTwain 扫描仪控件');
  end;
  FAppID.ProtocolMajor := TWON_PROTOCOLMAJOR;
  FAppID.ProtocolMinor := TWON_PROTOCOLMINOR;
  FAppID.SupportedGroups := DG_IMAGE_OR_CONTROL;
  strcopy(FAppID.Manufacturer, '上海维普软件工作室');
  strcopy(FAppID.ProductFamily, 'Delphi7专用扫描仪控件');
  strcopy(FAppID.ProductName, '扫描仪控件');
  FillChar(FDsID, Sizeof(TW_IDENTITY), 0);
  FTransferType := doNativeTransfer;
  hDSMDLL := 0;
  lpDSM_Entry := nil;
  FHooked := False;
  FAutoFeed := False;
  FIsDSMOpen := False;
  FIsDSOpen := False;
  FIsDSEnabled := False;
end;

destructor TCnTwain.Destroy;
begin
  Terminate;
  inherited Destroy;
end;

procedure TCnTwain.HookWin;
begin
  OldWndProc := TFarProc(GetWindowLong(FHandle, GWL_WNDPROC));
  NewWndProc := MakeObjectInstance(WndProc);
  SetWindowLong(FHandle, GWL_WNDPROC, LongInt(NewWndProc));
  FHooked := True;
end;

procedure TCnTwain.UnHookWin;
begin
  if not fHooked then
    exit;
  SetWindowLong(FHandle, GWL_WNDPROC, LongInt(OldWndProc));
  if AsSigned(NewWndProc) then
    FreeObjectInstance(NewWndProc);
  NewWndProc := nil;
  FHooked := False;
end;

procedure TCnTwain.WndProc(var Message: TMessage);
begin
  if not IsDSOpen or not ProcessTWMessage(Message, FHandle) then
  begin
//    if Message.Msg = PM_XFERDONE then ;
    Message.Result := CallWindowProc(OldWndProc, FHandle, Message.Msg, Message.wParam, Message.lParam);
  end;
end;

function TCnTwain.ProcessTWMessage(var aMsg: TMessage; TwhWnd: THandle): Boolean;
var
  twRC: TW_UINT16;
  twEv: TW_EVENT;
  theMsg: TMsg;
begin     // Here Something delicacy that MSG of C++ and TMessage of Delphi are not Same.
  twRC := TWRC_NOTDSEVENT;
  if IsDSOpen then
  begin
    FillChar(twEv, Sizeof(TW_EVENT), #0);
    FillChar(theMsg, Sizeof(TMsg), #0);
    theMsg.hwnd := TwhWnd;
    theMsg.message := aMsg.Msg;
    theMsg.wParam := aMsg.WParam;
    theMsg.lParam := aMsg.LParam;
    twEv.pEvent := @theMsg;
    twRC := lpDSM_Entry(@FappID, @FdsID, DG_CONTROL, DAT_EVENT, MSG_PROCESSEVENT, @twEv);
    aMsg.Msg := theMsg.message;
    aMsg.WParam := theMsg.wParam;
    aMsg.LParam := theMsg.lParam;
    aMsg.Result := twRC;
    case twEv.TWMessage of
      MSG_XFERREADY:
        TransferImage;
      MSG_CLOSEDSREQ, MSG_CLOSEDSOK:
        Terminate;
    end;
  end;
  Result := twRC = TWRC_DSEVENT;
end;

procedure TCnTwain.TransferImage;
begin
  case FTransferType of
    doNativeTransfer:
      NativeTransfer;
    doFileTransfer:
      FileTransfer;
    doMemTransfer:
      MemoryTransfer;
  end; 
  //
end;

procedure TCnTwain.NativeTransfer;
var
  twPendingXfer: TW_PENDINGXFERS;
  twRC, twRC2: TW_UINT16;
  hBitMap: TW_UINT32;
  hbm_acq: THandle;
begin
  hBitMap := 0;
  FillChar(twPendingXfer, sizeof(TW_PENDINGXFERS), #0);
  repeat
    twRC := lpDSM_Entry(@FappID, @FdsID, DG_IMAGE, DAT_IMAGENATIVEXFER, MSG_GET, @hBitMap);
    case twRC of
      TWRC_XFERDONE:
        begin
          hbm_acq := hBitMap;
          twRC2 := lpDSM_Entry(@FappID, @FdsID, DG_CONTROL, DAT_PENDINGXFERS, MSG_ENDXFER, @twPendingXfer);
          if twRC2 <> TWRC_SUCCESS then
            DoTwMessage('DG_CONTROL/DAT_PENDINGXFERS/MSG_ENDXFER', False);
          if twPendingXfer.Count = 0 then
            if (hbm_acq <> 0) and (GlobalLock(hbm_acq) <> nil) then
            begin
              Terminate;
              GlobalUnlock(hbm_acq);
            end;
          if hbm_acq > VALID_HANDLE then
          begin
            DoXferDone(hbm_acq);
            GlobalFree(hbm_acq);
          end
          else
            DoXferDone(0);
        end;
      TWRC_CANCEL:
        begin
               // DoTwMessage('User Cancel. (DG_IMAGE/DAT_IMAGENATIVEXFER/MSG_GET)', False);
          twRC2 := lpDSM_Entry(@FappID, @FdsID, DG_CONTROL, DAT_PENDINGXFERS, MSG_ENDXFER, @twPendingXfer);
          if twRC2 <> TWRC_SUCCESS then
            DoTwMessage('DG_CONTROL/DAT_PENDINGXFERS/MSG_ENDXFER', False);
          if twPendingXfer.Count = 0 then
            Terminate;
          DoXferDone(0);
        end;
    else
      twRC2 := lpDSM_Entry(@FappID, @FdsID, DG_CONTROL, DAT_PENDINGXFERS, MSG_ENDXFER, @twPendingXfer);
      if twRC2 <> TWRC_SUCCESS then
        DoTwMessage('DG_CONTROL/DAT_PENDINGXFERS/MSG_ENDXFER', False);
      if twPendingXfer.Count = 0 then
        Terminate;
      DoXferDone(0);
    end;
  until twPendingXfer.count = 0;
end;

procedure TCnTwain.FileTransfer;
var
  twPendingXfer: TW_PENDINGXFERS;
  SetupMsgGet, setup: TW_SETUPFILEXFER;
  ofs: OFSTRUCT;
  hF: THandle;
  twRC, twRC2: TW_UINT16;
  hbm_acq: THandle;
  header: BITMAPFILEHEADER;
  dwSize: DWord;
  ptr: PChar;
  count: TW_UINT32;
  num: TW_UINT16;
  FFileName: string;
begin
  FillChar(twPendingXfer, sizeof(TW_PENDINGXFERS), #0);
  FillChar(SetupMsgGet, sizeof(TW_SETUPFILEXFER), #0);
  FillChar(setup, sizeof(TW_SETUPFILEXFER), #0);
  FillChar(ofs, sizeof(OFSTRUCT), #0);
  repeat
    FFileName := '';
    if Assigned(FOnFileNameNeeded) then
      FOnFileNameNeeded(Self, FFileName);
    if FFileName <> '' then
      StrPCopy(setup.FileName, FFileName)
    else
    begin
      lpDSM_Entry(@FappID, @FdsID, DG_CONTROL, DAT_SETUPFILEXFER, MSG_GET, @SetupMsgGet);
      StrCopy(setup.FileName, SetupMsgGet.FileName);
    end;
    setup.Format := TWFF_BMP;
    setup.VRefNum := 0;
    hF := OpenFile(PAnsiChar({$IFDEF UNICODE}AnsiString{$ELSE}string{$ENDIF}(setup.Filename)), ofs, OF_CREATE);
    if hF = HFILE_ERROR then
    begin
      DoTwMessage('Unable to create file for file transfer', False);
      twRC := TWRC_FAILURE;
    end
    else
    begin
      _lclose(hF);
      twRC := lpDSM_Entry(@FappID, @FdsID, DG_CONTROL, DAT_SETUPFILEXFER, MSG_SET, @setup);
      if twRC <> TWRC_SUCCESS then
        DoTwMessage('DG_CONTROL/DAT_SETUPFILEXFER/MSG_SET', False)
      else
        twRC := lpDSM_Entry(@FappID, @FdsID, DG_IMAGE, DAT_IMAGEFILEXFER, MSG_GET, nil);
      case twRC of
        TWRC_XFERDONE:
          begin
            FillChar(ofs, sizeof(OFSTRUCT), #0);
            FillChar(header, sizeof(BITMAPFILEHEADER), #0);
            hF := OpenFile(PAnsiChar({$IFDEF UNICODE}AnsiString{$ELSE}string{$ENDIF}(setup.FileName)), ofs, OF_READ);
            hbm_acq := 0;
            if hF <> Longword(-1) then
            begin
              num := $8000;
              dwSize := GetFileSize(hF, nil);
              _lread(hF, @header, sizeof(BITMAPFILEHEADER));
              Dec(dwSize, sizeof(BITMAPFILEHEADER));
              if header.bfSize = 0 then
                header.bfSize := dwSize;
              hbm_acq := GlobalAlloc(GHND, header.bfSize);
              if hbm_acq <> 0 then
              begin
                ptr := GlobalLock(hbm_acq); 
               //for count:=(header.bfSize-sizeof(BITMAPFILEHEADER)) downto count; count-=num, ptr+=num)
                count := header.bfSize - sizeof(BITMAPFILEHEADER);
                while count > 0 do
                begin
                  if count < num then
                    num := count;
                  _lread(hF, ptr, num);
                  Dec(count, num);
                  Inc(ptr, num);
                end;
                GlobalUnlock(hbm_acq);
                GlobalFree(hbm_acq);
              end;
              _lclose(hF);
            end;
            twRC2 := lpDSM_Entry(@FappID, @FdsID, DG_CONTROL, DAT_PENDINGXFERS, MSG_ENDXFER, @twPendingXfer);
            if twRC2 <> TWRC_SUCCESS then
              DoTwMessage('DG_CONTROL / DAT_PENDINGXFERS / MSG_ENDXFER', False);
            if twPendingXfer.Count = 0 then
              Terminate;
            DoXferDone(hbm_acq);
          end;
        TWRC_CANCEL:
          begin
           // DoTwMessage('User Cancel. (DG_IMAGE/DAT_IMAGENATIVEXFER/MSG_GET)', False);
            twRC2 := lpDSM_Entry(@FappID, @FdsID, DG_CONTROL, DAT_PENDINGXFERS, MSG_ENDXFER, @twPendingXfer);
            if twRC2 <> TWRC_SUCCESS then
              DoTwMessage('DG_CONTROL/DAT_PENDINGXFERS/MSG_ENDXFER', False);
            if twPendingXfer.Count = 0 then
              Terminate;
            DoXferDone(0);
          end;
      else
        twRC2 := lpDSM_Entry(@FappID, @FdsID, DG_CONTROL, DAT_PENDINGXFERS, MSG_ENDXFER, @twPendingXfer);
        if twRC2 <> TWRC_SUCCESS then
          DoTwMessage('DG_CONTROL/DAT_PENDINGXFERS/MSG_ENDXFER', False);
        if twPendingXfer.Count = 0 then
          Terminate;
        DoXferDone(0);
      end;
    end;
  until((twPendingXfer.Count = 0) or (twRC = TWRC_FAILURE));
end;

procedure TCnTwain.MemoryTransfer;
var
  twPendingXfer: TW_PENDINGXFERS;
  info: TW_IMAGEINFO;
  twRC, twRC2: TW_UINT16;
  size: TW_UINT32;
  setup: TW_SETUPMEMXFER;
  blocks, index: integer;
  hbm_acq: THandle;
  pdib: pBITMAPINFO;
  cap: TW_CAPABILITY;
  pOneV: pTW_ONEVALUE;
  Units, PixelFlavor: TW_UINT16;
  XRes, YRes: Double;
  pal: TW_PALETTE8;
  ptr: PByte;
  xfer: TW_IMAGEMEMXFER;
begin
  FillChar(twPendingXfer, sizeof(TW_PENDINGXFERS), #0);
  FillChar(info, sizeof(TW_IMAGEINFO), #0);
  FillChar(setup, sizeof(TW_SETUPMEMXFER), #0);
  FillChar(pal, sizeof(TW_PALETTE8), #0);
  FillChar(xfer, sizeof(TW_IMAGEMEMXFER), #0);
  repeat
    twRC := lpDSM_Entry(@FappID, @FdsID, DG_IMAGE, DAT_IMAGEINFO, MSG_GET, @info);
    if twRC <> TWRC_SUCCESS then
      DoTwMessage('DG_IMAGE/DAT_IMAGEINFO/MSG_GET', False)
    else
    begin
      size := (((info.ImageWidth * info.BitsPerPixel + 31) div 8) * info.ImageLength);
      lpDSM_Entry(@FappID, @FdsID, DG_CONTROL, DAT_SETUPMEMXFER, MSG_GET, @setup);
      blocks := size div setup.Preferred;
      size := (TW_UINT32(blocks) + 1) * setup.Preferred;
      hbm_acq := GlobalAlloc(GHND, size + sizeof(BITMAPINFOHEADER) + 256 * sizeof(RGBQUAD));
      if hbm_acq = 0 then
        DoTwMessage('GlobalAlloc Failed in DoMemTransfer', False)
      else
      begin
        pdib := GlobalLock(hbm_acq); 
        // fill in the image information
        pdib^.bmiHeader.biSize := sizeof(BITMAPINFOHEADER);
        pdib^.bmiHeader.biWidth := info.ImageWidth;
        pdib^.bmiHeader.biHeight := info.ImageLength; 
        // Only 1 is supported
        pdib^.bmiHeader.biPlanes := 1;
        pdib^.bmiHeader.biBitCount := info.BitsPerPixel; 
        // This application does not support compression
        pdib^.bmiHeader.biCompression := BI_RGB;
        pdib^.bmiHeader.biSizeImage := size; 
        // Get Units and calculate PelsPerMeter
        cap.Cap := ICAP_UNITS;
        cap.ConType := TW_UINT16(TWON_DONTCARE16);
        cap.hContainer := 0;
        twRC := lpDSM_Entry(@FappID, @FdsID, DG_CONTROL, DAT_CAPABILITY, MSG_GETCURRENT, @cap);
        if twRC <> TWRC_SUCCESS then
        begin
          // raise ETwainError.Create('DG_CONTROL/DAT_CAPABILITY/MSG_GETCURRENT');
          pdib^.bmiHeader.biXPelsPerMeter := 0;
          pdib^.bmiHeader.biYPelsPerMeter := 0;
        end
        else
        begin
          pOneV := GlobalLock(cap.hContainer);
          Units := pOneV^.Item;
          GlobalUnlock(cap.hContainer);
          GlobalFree(cap.hContainer);
          XRes := FIX32ToFloat(info.XResolution);
          YRes := FIX32ToFloat(info.YResolution);
          case Units of
            TWUN_INCHES:
              begin
                pdib^.bmiHeader.biXPelsPerMeter := Trunc((XRes * 2.54) * 100);
                pdib^.bmiHeader.biYPelsPerMeter := Trunc((YRes * 2.54) * 100);
              end;
            TWUN_CENTIMETERS:
              begin
                pdib^.bmiHeader.biXPelsPerMeter := Trunc(XRes * 100);
                pdib^.bmiHeader.biYPelsPerMeter := Trunc(YRes * 100);
              end;
          else
            begin
              pdib^.bmiHeader.biXPelsPerMeter := 0;
              pdib^.bmiHeader.biYPelsPerMeter := 0;
            end;
          end;
          case info.PixelType of
            TWPT_BW:
              begin
                pdib^.bmiHeader.biClrUsed := 2;
                pdib^.bmiHeader.biClrImportant := 0;
                cap.Cap := ICAP_PIXELFLAVOR;
                cap.ConType := TW_UINT16(TWON_DONTCARE16);
                cap.hContainer := 0;
                twRC := lpDSM_Entry(@FappID, @FdsID, DG_CONTROL, DAT_CAPABILITY, MSG_GETCURRENT, @cap);
                if twRC <> TWRC_SUCCESS then
                  PixelFlavor := TWPF_CHOCOLATE
                else
                begin
                  if cap.ConType <> TWON_ONEVALUE then
                    PixelFlavor := TWPF_CHOCOLATE
                  else
                  begin
                    pOneV := GlobalLock(cap.hContainer);
                    PixelFlavor := TW_UINT16(pOneV^.Item);
                    GlobalUnlock(cap.hContainer);
                  end;
                  GlobalFree(cap.hContainer);
                end;
                if PixelFlavor = 0 then
                begin
                  pdib^.bmiColors[0].rgbRed := 0;
                  pdib^.bmiColors[0].rgbGreen := 0;
                  pdib^.bmiColors[0].rgbBlue := 0;
                  pdib^.bmiColors[0].rgbReserved := 0;
                  index := 1;
                  pdib^.bmiColors[index].rgbRed := $00FF;
                  pdib^.bmiColors[index].rgbGreen := $00FF;
                  pdib^.bmiColors[index].rgbBlue := $00FF;
                  pdib^.bmiColors[index].rgbReserved := 0;
                end
                else
                begin
                  pdib^.bmiColors[0].rgbRed := $00FF;
                  pdib^.bmiColors[0].rgbGreen := $00FF;
                  pdib^.bmiColors[0].rgbBlue := $00FF;
                  pdib^.bmiColors[0].rgbReserved := 0;
                  index := 1;
                  pdib^.bmiColors[index].rgbRed := 0;
                  pdib^.bmiColors[index].rgbGreen := 0;
                  pdib^.bmiColors[index].rgbBlue := 0;
                  pdib^.bmiColors[index].rgbReserved := 0;
                end;
              end;
            TWPT_GRAY:
              begin
                pdib^.bmiHeader.biClrUsed := 256;
                for index := 0 to 255 do
                begin
                  pdib^.bmiColors[index].rgbRed := BYTE(index);
                  pdib^.bmiColors[index].rgbGreen := BYTE(index);
                  pdib^.bmiColors[index].rgbBlue := BYTE(index);
                  pdib^.bmiColors[index].rgbReserved := 0;
                end;
              end;
            TWPT_RGB:
              pdib^.bmiHeader.biClrUsed := 0;
          else
            twRC := lpDSM_Entry(@FappID, @FdsID, DG_IMAGE, DAT_PALETTE8, MSG_GET, @pal);
            if twRC <> TWRC_SUCCESS then
            begin
                 // raise ETwainError.Create('DG_IMAGE/DAT_PALETTE8/MSG_GET -- defaulting to 256 gray image palette');
              pdib^.bmiHeader.biClrImportant := 0;
              pdib^.bmiHeader.biClrUsed := 256;
              for index := 0 to pal.NumColors - 1 do
              begin
                pdib^.bmiColors[index].rgbRed := BYTE(index);
                pdib^.bmiColors[index].rgbGreen := BYTE(index);
                pdib^.bmiColors[index].rgbBlue := BYTE(index);
                pdib^.bmiColors[index].rgbReserved := 0;
              end;
            end
            else
            begin
              pdib^.bmiHeader.biClrImportant := 0;
              pdib^.bmiHeader.biClrUsed := pal.NumColors;
              for index := 0 to pal.NumColors - 1 do
              begin
                pdib^.bmiColors[index].rgbRed := pal.Colors[index].Channel1;
                pdib^.bmiColors[index].rgbGreen := pal.Colors[index].Channel2;
                pdib^.bmiColors[index].rgbBlue := pal.Colors[index].Channel3;
                pdib^.bmiColors[index].rgbReserved := 0;
              end;
            end;
          end;
          ptr := PByte(pdib);
          Inc(ptr, sizeof(BITMAPINFOHEADER));
          Inc(ptr, pdib^.bmiHeader.biClrUsed * sizeof(RGBQUAD));
          twRC := lpDSM_Entry(@FappID, @FdsID, DG_CONTROL, DAT_SETUPMEMXFER, MSG_GET, @setup);
          if twRC <> TWRC_SUCCESS then
            DoTwMessage('DG_CONTROL/DAT_SETUPMEMXFER/MSG_GET', False)
          else
          begin
            // we will use a pointer to shared memory
            xfer.Memory.Flags := TWMF_APPOWNS or TWMF_POINTER;
            xfer.Memory.Length := setup.Preferred;
            xfer.Memory.TheMem := ptr; 
            // transfer the data -- loop until done or canceled
            repeat
              twRC := lpDSM_Entry(@FappID, @FdsID, DG_IMAGE, DAT_IMAGEMEMXFER, MSG_GET, @xfer);
              case twRC of
                TWRC_SUCCESS:
                  begin
                    Inc(ptr, xfer.BytesWritten);
                    xfer.Memory.TheMem := ptr;
                  end;
                TWRC_XFERDONE:
                  begin
                    GlobalUnlock(hbm_acq);
                    FlipBitMap(FHandle, hbm_acq, info.PixelType);
                    twRC2 := lpDSM_Entry(@FappID, @FdsID, DG_CONTROL, DAT_PENDINGXFERS, MSG_ENDXFER, @twPendingXfer);
                    if twRC2 <> TWRC_SUCCESS then
                      DoTwMessage('DG_CONTROL / DAT_PENDINGXFERS / MSG_ENDXFER', False);
                    if twPendingXfer.Count = 0 then
                      Terminate;
                    DoXferDone(hbm_acq);
                    GlobalFree(hbm_acq);
                  end;
                TWRC_CANCEL:
                  begin
                   // DoTwMessage('User Cancel. (DG_IMAGE/DAT_IMAGENATIVEXFER/MSG_GET)', False);
                    twRC2 := lpDSM_Entry(@FappID, @FdsID, DG_CONTROL, DAT_PENDINGXFERS, MSG_ENDXFER, @twPendingXfer);
                    if twRC2 <> TWRC_SUCCESS then
                      DoTwMessage('DG_CONTROL / DAT_PENDINGXFERS / MSG_ENDXFER', False);
                    GlobalUnlock(hbm_acq);
                    GlobalFree(hbm_acq);
                    if twPendingXfer.Count = 0 then
                      Terminate;
                    DoXferDone(0);
                  end;
              else
                twRC2 := lpDSM_Entry(@FappID, @FdsID, DG_CONTROL, DAT_PENDINGXFERS, MSG_ENDXFER, @twPendingXfer);
                if twRC2 <> TWRC_SUCCESS then
                  DoTwMessage('DG_CONTROL / DAT_PENDINGXFERS / MSG_ENDXFER', False);
                GlobalUnlock(hbm_acq);
                GlobalFree(hbm_acq);
                if twPendingXfer.Count = 0 then
                  Terminate;
                DoXferDone(0);
              end;
            until(twRC <> TWRC_SUCCESS);
          end;  // if twRC <> TWRC_SUCCESS then DoTwMessage('DG_CONTROL/DAT_SETUPMEMXFER/MSG_GET', False) else begin
        end;
      end;   // hbm_acq = 0
    end;  // twRC <> TWRC_SUCCESS
  until twPendingXfer.count = 0;
end;

procedure TCnTwain.DoXferDone(hDib: THandle);
var
  lpDib, lpBi: PBITMAPINFOHEADER;
  lpBits: Pointer;
  dwColorTableSize: TW_UINT32;
  hBitMap: TW_UINT32;
  hDibPal: THandle;
  DC: HDC;
  bmp: TBitmap;
begin
  if not Assigned(FOnCapture) then
    Exit;
  if hDib = 0 then
  begin
    FOnCapture(Self, nil);
    Exit;
  end;
  lpDib := GlobalLock(hDib);
  if lpDib = nil then
  begin
    DoTwMessage('Could Not Lock Bitmap Memory.', False);
    Exit;
  end;
  lpBi := lpDib;
  dwColorTableSize := DibNumColors(lpDib) * sizeof(RGBQUAD);
  lpBits := lpDib;
  Inc(pByte(lpBits), lpBi^.biSize + dwColorTableSize);
  DC := GetDC(FHandle);
  hDibPal := CreateBIPalette(lpBi);
  if hDibPal <> 0 then
  begin
    SelectPalette(DC, hDibPal, False);
    RealizePalette(DC);
  end;
  if lpDib^.biBitCount = 1 then
  begin
    hBitMap := CreateBitmap(lpDib^.biWidth, lpDib^.biHeight, 1, 1, lpBits);
    if hBitMap <> 0 then
      SetDIBits(DC, hBitMap, 0, lpDib^.biHeight, lpBits, pBITMAPINFO(lpDib)^, DIB_RGB_COLORS);
  end
  else
    hBitMap := CreateDIBitmap(DC, lpDib^, CBM_INIT, lpBits, pBITMAPINFO(lpDib)^, DIB_RGB_COLORS);
  GlobalUnlock(hDib);
  ReleaseDC(FHandle, DC);
  bmp := TBitmap.Create;
  bmp.Handle := hBitMap;
  FOnCapture(Self, bmp);
  bmp.Free;
end;

procedure TCnTwain.DoTwMessage(Msg: string; TerminateDS: Boolean = True);
begin
  if TerminateDS then
    Terminate;
  if Assigned(FOnTwMessage) then
    FOnTwMessage(Self, Msg);
end;

function TCnTwain.OpenDSM: TW_UINT16;
begin
  Result := TWRC_FAILURE;
  if IsDSMOpen then
    Exit;
  hDSMDLL := LoadLibrary('TWAIN_32.DLL');
  if hDSMDLL <> 0 then
    @lpDSM_Entry := GetProcAddress(hDSMDLL, 'DSM_Entry');
  if (hDSMDLL = 0) or (@lpDSM_Entry = nil) then
    DoTwMessage('Error in Open, LoadLibrary, or GetProcAddress.');
  Result := lpDSM_Entry(@FAppID, nil, DG_CONTROL, DAT_PARENT, MSG_OPENDSM, @FHandle);
  if Result = TWRC_SUCCESS then
    FIsDSMOpen := True
  else
    DoTwMessage('Error Open DSM. (DG_CONTROL/DAT_PARENT/MSG_OPENDSM)');
end;

function TCnTwain.CloseDSM: TW_UINT16;
begin
  Result := TWRC_FAILURE;
  if IsDSMOpen then
  begin
    Result := lpDSM_Entry(@FAppID, nil, DG_CONTROL, DAT_PARENT, MSG_CLOSEDSM, @FHandle);
    if hDSMDLL <> 0 then
    begin
      FreeLibrary(hDSMDLL);
      hDSMDLL := 0;
    end;
    if Result <> TWRC_SUCCESS then
      DoTwMessage('Error Close DSM. (DG_CONTROL/DAT_PARENT/MSG_CLOSEDSM)');
    FdsID.Id := 0;
  end;
  FIsDSMOpen := False;
end;

function TCnTwain.OpenDS: TW_UINT16;
begin
  Result := TWRC_FAILURE;
  if IsDSMOpen then
    if not IsDSOpen then
    begin
      Result := lpDSM_Entry(@FAppID, nil, DG_CONTROL, DAT_IDENTITY, MSG_OPENDS, @FdsID);
      if Result = TWRC_SUCCESS then
      begin
        FIsDSOpen := True;
        HookWin;
      end
      else
        DoTwMessage('Error Open DS. (DG_CONTROL/DAT_IDENTITY/MSG_OPENDS)');
    end
    else
      DoTwMessage('Can not Open DS while It is Openning')
  else
    DoTwMessage('Can not Open DS while DSM not Openning');
end;

function TCnTwain.CloseDS: TW_UINT16;
begin
  Result := TWRC_FAILURE;
  if IsDSOpen then
    if not IsDSEnabled then
    begin
      Result := lpDSM_Entry(@FAppID, nil, DG_CONTROL, DAT_IDENTITY, MSG_CLOSEDS, @FdsID);
      if Result = TWRC_SUCCESS then
      begin
        FIsDSOpen := False;
        UnHookWin;
      end
      else
        DoTwMessage('Error Close DS. (DG_CONTROL/DAT_IDENTITY/MSG_CLOSEDS)');
      FillChar(FdsID, Sizeof(TW_IDENTITY), #0);
    end
    else
      DoTwMessage('Can not Close DS while DS is Enabled');
  FIsDSOpen := False;
end;

function TCnTwain.XferMechDS: TW_UINT16;
var
  cap: TW_CAPABILITY;
  pval: pTW_ONEVALUE;
begin
  Result := TWRC_FAILURE;
  cap.Cap := ICAP_XFERMECH;
  cap.ConType := TWON_ONEVALUE;
  cap.hContainer := GlobalAlloc(GHND, Sizeof(TW_ONEVALUE));
  if cap.hContainer = 0 then
  begin
    DoTwMessage('Memory Allocation Failed. (MSG_SET/ICAP_XFERMECH)');
    Exit;
  end;
  pval := GlobalLock(cap.hContainer);
  pval^.ItemType := TWTY_UINT16;
  case FTransferType of
    doNativeTransfer:
      pval^.Item := TWSX_NATIVE;
    doFileTransfer:
      pval^.Item := TWSX_FILE;
    doMemTransfer:
      pval^.Item := TWSX_MEMORY;
  end;
  GlobalUnlock(cap.hContainer);
  Result := lpDSM_Entry(@FAppID, @FdsID, DG_CONTROL, DAT_CAPABILITY, MSG_SET, @cap);
  GlobalFree(cap.hContainer);
  if Result <> TWRC_SUCCESS then
    DoTwMessage('Error XferMech DS. (DG_CONTROL/DAT_CAPABILITY/MSG_SET)');
end;

function TCnTwain.AutoFeedDS: TW_UINT16;
var
  cap: TW_CAPABILITY;
  pval: pTW_ONEVALUE;
begin
  Result := TWRC_SUCCESS;
  if not FAutoFeed then
    Exit; 
  // Get Feeder Enabled
  FillChar(cap, Sizeof(TW_CAPABILITY), 0);
  cap.Cap := CAP_FEEDERENABLED;
  cap.ConType := TWON_ONEVALUE;
  Result := lpDSM_Entry(@FAppID, @FdsID, DG_CONTROL, DAT_CAPABILITY, MSG_GET, @cap);
  if Result <> TWRC_SUCCESS then
  begin
    GlobalFree(cap.hContainer);
    DoTwMessage('Error get AutoFeed. (DG_CONTROL/DAT_CAPABILITY/MSG_GET)');
    Exit;
  end;
  pval := GlobalLock(cap.hContainer);
  if pval^.Item <> 0 then
  begin  // Feeder Enabled
    GlobalUnlock(cap.hContainer);
    GlobalFree(cap.hContainer);
  end
  else
  begin
    // Set Feeder Enabled
    pval^.ItemType := TWTY_BOOL;
    pval^.Item := 1;   // TRUE
    Result := lpDSM_Entry(@FAppID, @FdsID, DG_CONTROL, DAT_CAPABILITY, MSG_SET, @cap);
    GlobalFree(cap.hContainer);
    if Result = TWRC_SUCCESS then
    begin
      // Verify Feeder Enabled
      Result := lpDSM_Entry(@FAppID, @FdsID, DG_CONTROL, DAT_CAPABILITY, MSG_GET, @cap);
      if Result = TWRC_SUCCESS then
      begin
        pval := GlobalLock(cap.hContainer);
        if pval^.Item = 0 then
          Result := TWRC_FAILURE;  // not set
        GlobalUnlock(cap.hContainer);
        GlobalFree(cap.hContainer);
      end
      else
        DoTwMessage('Error Get AutoFeed. (DG_CONTROL, DAT_CAPABILITY, MSG_GET)');
    end
    else
      DoTwMessage('Error Get AutoFeed. (DG_CONTROL, DAT_CAPABILITY, MSG_SET)');
  end;
  if Result = TWRC_SUCCESS then
  begin
    // Get AutoFeed
    cap.Cap := CAP_AUTOFEED;
    cap.ConType := TWON_ONEVALUE;
    Result := lpDSM_Entry(@FAppID, @FdsID, DG_CONTROL, DAT_CAPABILITY, MSG_GET, @cap);
    pval := GlobalLock(cap.hContainer);
    if pval^.Item <> 0 then
    begin  // already auto feed
      GlobalUnlock(cap.hContainer);
      GlobalFree(cap.hContainer);
    end
    else
    begin
      // Set AutoFeed
      pval^.ItemType := TWTY_BOOL;
      pval^.Item := 1;  // TRUE;
      GlobalUnlock(cap.hContainer);
      Result := lpDSM_Entry(@FAppID, @FdsID, DG_CONTROL, DAT_CAPABILITY, MSG_SET, @cap);
      GlobalFree(cap.hContainer);
      if Result = TWRC_SUCCESS then
      begin
        // Verify AutoFeed
        Result := lpDSM_Entry(@FAppID, @FdsID, DG_CONTROL, DAT_CAPABILITY, MSG_GET, @cap);
        if Result = TWRC_SUCCESS then
        begin
          pval := GlobalLock(cap.hContainer);
          if pval^.Item <> 0 then
            Result := TWRC_FAILURE;   // not been set
          GlobalUnlock(cap.hContainer);
          GlobalFree(cap.hContainer);
        end;
      end
      else
        DoTwMessage('Error set AutoFeed. (DG_CONTROL, DAT_CAPABILITY, MSG_SET)');
    end;
  end; 
//  AutoFeedBOOL := Result = TWRC_SUCCESS;
end;

function TCnTwain.EnableDS(Show: Boolean): TW_UINT16;
begin
  Result := TWRC_FAILURE;
  if IsDSOpen then
    if not IsDSEnabled then
    begin
      twUI.hParent := FHandle;
      twUI.ModalUI := 0;  // Mac Only..
      if Show then
        twUI.ShowUI := 1
      else
        twUI.ShowUI := 0;
      Result := lpDSM_Entry(@FAppID, @FdsID, DG_CONTROL, DAT_USERINTERFACE, MSG_ENABLEDS, @twUI);
      if Result = TWRC_SUCCESS then
        FIsDSEnabled := True
      else
        DoTwMessage('Error Enable DS. (DG_CONTROL/DAT_USERINTERFACE/MSG_ENABLEDS)');
    end
    else
      DoTwMessage('Can not Enable DS while it already Enabled')
  else
    DoTwMessage('Can not Enable DS while DS is not Openning');
end;

function TCnTwain.DisableDS: TW_UINT16;
begin
  Result := TWRC_FAILURE;
  if IsDSEnabled then
  begin
    twUI.hParent := FHandle;
    twUI.ShowUI := TW_BOOL(TWON_DONTCARE8);
    Result := lpDSM_Entry(@FAppID, @FdsID, DG_CONTROL, DAT_USERINTERFACE, MSG_DISABLEDS, @twUI);
    if Result = TWRC_SUCCESS then
      FIsDSEnabled := False
    else
      DoTwMessage('Error Disable DS. (DG_CONTROL/DAT_USERINTERFACE/MSG_DISABLEDS)');
  end;
  FIsDSEnabled := False;
end;

function TCnTwain.SelectDS: TW_UINT16;
var
  NewDsID: TW_IDENTITY;
begin
  Result := TWRC_FAILURE;
  NewDsID.Id := 0;
  NewDsID.ProductName[0] := #0;
  if not IsDSOpen then
  begin
    Result := lpDSM_Entry(@FAppID, nil, DG_CONTROL, DAT_IDENTITY, MSG_USERSELECT, @NewDsID);
    if Result = TWRC_SUCCESS then
      FdsID := NewDsID;
  end
  else
    DoTwMessage('Can not Select New DS while DS is Openning');
end;

function TCnTwain.Acquire(Show: Boolean): TW_UINT16;
begin
  Result := TWRC_FAILURE;
  if not IsDSMOpen then
    Result := OpenDSM;
  if Result <> TWRC_SUCCESS then
    Exit;
  if not IsDSOpen then
    Result := OpenDS;
  if Result <> TWRC_SUCCESS then
    Exit;
  Result := XferMechDS;
  if Result <> TWRC_SUCCESS then
    Exit;
  Result := AutoFeedDS;
  if Result <> TWRC_SUCCESS then
    Exit;
  if not IsDSEnabled then
    Result := EnableDS(Show);
end;

function TCnTwain.GetDSInfo(var DsID: TW_IDENTITY): TW_UINT16;
begin
  Result := TWRC_FAILURE;
  if not FIsDSMOpen then
  begin
    if OpenDSM <> TWRC_SUCCESS then
      Exit;
    if OpenDS <> TWRC_SUCCESS then
      Exit;
    DsID := FDsID;
    Result := TWRC_SUCCESS;
    CloseDS;
    CloseDSM;
  end
  else // DSM Openned.
 if FIsDSOpen then
  begin
    DsID := FDsID;
    Result := TWRC_SUCCESS;
  end
  else
  begin
    if OpenDS <> TWRC_SUCCESS then
      Exit;
    DsID := FDsID;
    CloseDS;
  end;
end;

function TCnTwain.GetComponentInfo(var DsID: TW_IDENTITY): TW_UINT16;
begin
  Result := TWRC_SUCCESS;
  DsID := FAppId;
end;

function TCnTwain.SelectSource: TW_UINT16;
begin
  Result := TWRC_FAILURE;
  if not IsDSMOpen then
    OpenDSM;
  if IsDSOpen then
  begin //Can't Do Select While DS is Openning!
    DoTwMessage('Can''t Do Select While DS is Openning.', False);
    Exit;
  end;
  Result := SelectDS;
  if IsDSMOpen then
    CloseDSM;
end;

procedure TCnTwain.Terminate;
begin
  DisableDS;
  CloseDS;
  CloseDSM;
end;

end.
