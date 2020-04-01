{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2020 CnPack 开发组                       }
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

unit CnASCommon;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：ActiveScript Host 服务单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：该单元定义了供 ActiveScript 使用的一些公共服务
* 开发平台：PWin2K SP3 + Delphi 7
* 兼容测试：PWin9X/2000/XP + Delphi 6/7 C++Builder 6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2003.10.31
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF WIN32}

uses
  Windows, SysUtils, Classes, Registry, IniFiles, CnASIDispatchProxy, CnCommon;

type
  ICnASRegistry = interface(IActiveScriptInvokable)
  ['{428BFF29-68CC-4376-ACEB-16B4670CA920}']
    procedure CloseKey; stdcall;
    function CreateKey(const Key: string): Boolean; stdcall;
    function DeleteKey(const Key: string): Boolean; stdcall;
    function DeleteValue(const Name: string): Boolean; stdcall;
    function GetAccess: LongWord; stdcall;
    function GetCurrentKey: HKEY; stdcall;
    function GetCurrentPath: string; stdcall;
    function GetDataSize(const ValueName: string): Integer; stdcall;
    function GetDataType(const ValueName: string): TRegDataType; stdcall;
    function GetKeyNames: OleVariant; stdcall;
    function GetLazyWrite: Boolean; stdcall;
    function GetRootKey: HKEY; stdcall;
    function GetValueNames: OleVariant; stdcall;
    function HasSubKeys: Boolean; stdcall;
    function KeyExists(const Key: string): Boolean; stdcall;
    function LoadKey(const Key, FileName: string): Boolean; stdcall;
    procedure MoveKey(const OldName, NewName: string; Delete: Boolean); stdcall;
    function OpenKey(const Key: string; CanCreate: Boolean): Boolean; stdcall;
    function OpenKeyReadOnly(const Key: String): Boolean; stdcall;
    function ReadBinaryData(const Name: string): OleVariant; stdcall;
    function ReadBool(const Name: string): Boolean; stdcall;
    function ReadCurrency(const Name: string): Currency; stdcall;
    function ReadDate(const Name: string): TDateTime; stdcall;
    function ReadDateTime(const Name: string): TDateTime; stdcall;
    function ReadFloat(const Name: string): Double; stdcall;
    function ReadInteger(const Name: string): Integer; stdcall;
    function ReadString(const Name: string): string; stdcall;
    function ReadTime(const Name: string): TDateTime; stdcall;
    function RegistryConnect(const UNCName: string): Boolean; stdcall;
    procedure RenameValue(const OldName, NewName: string); stdcall;
    function ReplaceKey(const Key, FileName, BackUpFileName: string): Boolean; stdcall;
    function RestoreKey(const Key, FileName: string): Boolean; stdcall;
    function SaveKey(const Key, FileName: string): Boolean; stdcall;
    procedure SetAccess(const Value: LongWord); stdcall;
    procedure SetLazyWrite(const Value: Boolean); stdcall;
    procedure SetRootKey(const Value: HKEY); stdcall;
    function UnLoadKey(const Key: string): Boolean; stdcall;
    function ValueExists(const Name: string): Boolean; stdcall;
    procedure WriteBinaryData(const Name: string; Value: OleVariant); stdcall;
    procedure WriteBool(const Name: string; Value: Boolean); stdcall;
    procedure WriteCurrency(const Name: string; Value: Currency); stdcall;
    procedure WriteDate(const Name: string; Value: TDateTime); stdcall;
    procedure WriteDateTime(const Name: string; Value: TDateTime); stdcall;
    procedure WriteExpandString(const Name, Value: string); stdcall;
    procedure WriteFloat(const Name: string; Value: Double); stdcall;
    procedure WriteInteger(const Name: string; Value: Integer); stdcall;
    procedure WriteString(const Name, Value: string); stdcall;
    procedure WriteTime(const Name: string; Value: TDateTime); stdcall;
    property Access: LongWord read GetAccess write SetAccess;
    property CurrentKey: HKEY read GetCurrentKey;
    property CurrentPath: string read GetCurrentPath;
    property LazyWrite: Boolean read GetLazyWrite write SetLazyWrite;
    property RootKey: HKEY read GetRootKey write SetRootKey;
  end;

{$ENDIF}

implementation

end.
