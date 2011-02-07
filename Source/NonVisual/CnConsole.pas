{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2011 CnPack 开发组                       }
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

unit CnConsole;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：控制台组件 TCnConsole 单元
* 单元作者：刘啸 (liuxiao@cnpack.org)
*           菩提
* 备    注：为 GUI 程序增加控制台
* 开发平台：PWinXP + Delphi 5.0
* 兼容测试：PWinXP + Delphi 5.0
* 本 地 化：该单元中无字符串资源
* 单元标识：$Id$
* 修改记录：2008.10.14 v1.1
*               菩提加入控制文本颜色的功能
*           2006.10.05 v1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows,
  CnClasses, CnConsts, CnCompConsts;

  {控制台文本的颜色常量}
   const
      tfBlue  =1;
      tfGreen =2;
      tfRed   =4;
      tfIntensity = 8;
      tfWhite = $f;
      
      tbBlue  =$10;
      tbGreen =$20;
      tbRed   =$40;
      tbIntensity = $80;

type
  TCnConsole = class(TCnComponent)
  private
    FConsoleTitle: string;
    FEnabled: Boolean;
    FConsoleHandle:THandle;
    procedure SetConsoleTitle(const Value: string);
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    procedure ResetConsole;
    {* 复位控制台,在属性Enabled为True时有效。}
    procedure SetTextColor(const aColor:WORD);
    {* 设置控制台的前景颜色}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    {* 为True时启动控制台，为False关闭控制台}
    property ConsoleTitle: string read FConsoleTitle write SetConsoleTitle;
    {* 控制台的标题}
  end;

implementation

{ TCnConsole }

constructor TCnConsole.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := False;
end;

destructor TCnConsole.Destroy;
begin
  if not (csDesigning in ComponentState) and FEnabled then
    FreeConsole;
  inherited;
end;

procedure TCnConsole.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnConsoleName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnConsoleComment;
end;

procedure TCnConsole.ResetConsole;
begin
  if csDesigning in ComponentState then
    Exit;

  if FEnabled then
  begin
    FreeConsole;
    AllocConsole;
    FConsoleHandle := GetStdHandle(STD_OUTPUT_HANDLE);
    if FConsoleTitle <> '' then
      Windows.SetConsoleTitle(PChar(FConsoleTitle));
  end;
end;

procedure TCnConsole.SetConsoleTitle(const Value: string);
begin
  FConsoleTitle := Value;
  if FEnabled and not (csDesigning in ComponentState) then
    if (FConsoleTitle <> '') or not (csLoading in ComponentState) then
      Windows.SetConsoleTitle(PChar(FConsoleTitle));
end;

procedure TCnConsole.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if csDesigning in ComponentState then
      Exit;
      
    if FEnabled then
    begin
      AllocConsole;
      SetConsoleTitle(PChar(FConsoleTitle));
      FConsoleHandle := GetStdHandle(STD_OUTPUT_HANDLE);
    end
    else
    begin
      FreeConsole;
    end;
  end;
end;

procedure TCnConsole.SetTextColor(const aColor: WORD);
begin
  if FEnabled then
  SetConsoleTextAttribute(FConsoleHandle, aColor);
end;

end.
