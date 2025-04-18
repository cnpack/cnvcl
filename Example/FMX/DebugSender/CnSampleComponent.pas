{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnSampleComponent;
{ |<PRE>
================================================================================
* 软件名称：CnPack 专家包
* 单元名称：示例组件单元
* 单元作者：CnPack 开发组 master@cnpack.org
* 备    注：
* 开发平台：Win7 + Delphi 5
* 兼容测试：未测试
* 本 地 化：该窗体中的字符串暂不符合本地化处理方式
* 修改记录：2021.08.07
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  System.Types, System.Classes, FMX.Controls, FMX.Forms, FMX.Dialogs,
  System.UITypes, FMX.Graphics {$IFDEF MSWINDOWS}, Winapi.Windows {$ENDIF};

type
  TCnDynIntArray = array of Integer;

  TCnSampleComponent = class(TComponent)
  private
    FHint: AnsiString;
    FAccChar: Char;
    FFloatValue: Double;
    FInt64Value: Int64;
    FHeight: Integer;
    FIntfValue: IUnknown;
    FCaption: string;
    FDynArray: TCnDynIntArray;
    FAnchorKind: TAnchorKind;
    FAnchors: TAnchors;
    FParent: TControl;
{$IFDEF MSWINDOWS}
    FArrayValue: TKeyboardState;
{$ENDIF}
    FOnClick: TNotifyEvent;
    FVarValue: Variant;
    FWideAccChar: WideChar;
    FWideHint: WideString;
    FPoint: TPoint;
{$IFDEF UNICODE}
    FUniStr: string;
{$ENDIF}
    FReadOnlyHint: AnsiString;
    FReadOnlyAccChar: Char;
    FReadOnlyFloatValue: Double;
    FReadOnlyInt64Value: Int64;
    FReadOnlyHeight: Integer;
    FReadOnlyIntfValue: IUnknown;
    FReadOnlyCaption: string;
    FReadOnlyDynArray: TCnDynIntArray;
    FReadOnlyAnchorKind: TAnchorKind;
    FReadOnlyAnchors: TAnchors;
    FReadOnlyParent: TControl;
{$IFDEF MSWINDOWS}
    FReadOnlyArrayValue: TKeyboardState;
{$ENDIF}
    FReadOnlyOnClick: TNotifyEvent;
    FReadOnlyVarValue: Variant;
    FReadOnlyWideAccChar: WideChar;
    FReadOnlyWideHint: WideString;
    FReadOnlyPoint: TPoint;
    FReadOnlyFont: TFont;
{$IFDEF UNICODE}
    FReadOnlyUniStr: string;
{$ENDIF}
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
{$IFDEF MSWINDOWS}
    property ArrayValue: TKeyboardState read FArrayValue write FArrayValue;
{$ENDIF}
    property DynArray: TCnDynIntArray read FDynArray write FDynArray;
{$IFDEF MSWINDOWS}
    property ReadOnlyArrayValue: TKeyboardState read FReadOnlyArrayValue;
{$ENDIF}
    property ReadOnlyDynArray: TCnDynIntArray read FReadOnlyDynArray;

  published
{   属性涵盖：
    tkInteger, tkChar, tkEnumeration, tkFloat,
    tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray,
    tkUString, tkClassRef, tkPointer, tkProcedure, tkMRecord
}

    property Height: Integer read FHeight write FHeight;
    property AccChar: Char read FAccChar write FAccChar;
    property AnchorKind: TAnchorKind read FAnchorKind write FAnchorKind;
    property FloatValue: Double read FFloatValue write FFloatValue;
    property Caption: string read FCaption write FCaption;
    property Anchors: TAnchors read FAnchors write FAnchors;
    property Parent: TControl read FParent write FParent;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property WideAccChar: WideChar read FWideAccChar write FWideAccChar;
    property Hint: AnsiString read FHint write FHint;
    property WideHint: WideString read FWideHint write FWideHint;
    property VarValue: Variant read FVarValue write FVarValue;
    property Point: TPoint read FPoint write FPoint;
    property IntfValue: IUnknown read FIntfValue write FIntfValue;
    property Int64Value: Int64 read FInt64Value write FInt64Value;
{$IFDEF UNICODE}
    property UniStr: string read FUniStr write FUniStr;
{$ENDIF}

    property ReadOnlyHeight: Integer read FReadOnlyHeight;
    property ReadOnlyAccChar: Char read FReadOnlyAccChar;
    property ReadOnlyAnchorKind: TAnchorKind read FReadOnlyAnchorKind;
    property ReadOnlyFloatValue: Double read FReadOnlyFloatValue;
    property ReadOnlyCaption: string read FReadOnlyCaption;
    property ReadOnlyAnchors: TAnchors read FReadOnlyAnchors;
    property ReadOnlyParent: TControl read FReadOnlyParent;
    property ReadOnlyOnClick: TNotifyEvent read FReadOnlyOnClick;
    property ReadOnlyWideAccChar: WideChar read FReadOnlyWideAccChar;
    property ReadOnlyHint: AnsiString read FReadOnlyHint;
    property ReadOnlyWideHint: WideString read FReadOnlyWideHint;
    property ReadOnlyVarValue: Variant read FReadOnlyVarValue;
    property ReadOnlyPoint: TPoint read FReadOnlyPoint;
    property ReadOnlyIntfValue: IUnknown read FReadOnlyIntfValue;
    property ReadOnlyInt64Value: Int64 read FReadOnlyInt64Value;
    property ReadOnlyFont: TFont read FReadOnlyFont;
{$IFDEF UNICODE}
    property ReadOnlyUniStr: string read FReadOnlyUniStr write FReadOnlyUniStr;
{$ENDIF}
  end;

implementation

{ TCnSampleComponent }

constructor TCnSampleComponent.Create(AOwner: TComponent);
var
  WStr: WideString;
begin
  inherited;
  WStr := '我';

  FHint := 'Ansi Hint';
{$IFDEF UNICODE}
  FAccChar := '吃';
{$ELSE}
  FAccChar := 'A';
{$ENDIF}
  FFloatValue := 3.1415926;
  FInt64Value := 9999999988888888;
  FHeight := 80;
  FIntfValue := nil;
  FCaption := 'Caption';

  FAnchorKind := TAnchorKind.akRight;
  FAnchors := [TAnchorKind.akLeft, TAnchorKind.akBottom];
  FParent := nil;

{$IFDEF MSWINDOWS}
  FArrayValue[0] := 10;
{$ENDIF}

  FOnClick := nil;
  FVarValue := 0;
  FWideAccChar := WStr[1];
  FWideHint := 'Wide Hint';
  FPoint.x := 10;
  FPoint.y := 20;

  FReadOnlyFont := TFont.Create;
end;

destructor TCnSampleComponent.Destroy;
begin
  FReadOnlyFont.Free;
  inherited;
end;

end.
 