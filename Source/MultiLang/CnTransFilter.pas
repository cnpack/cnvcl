{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
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

unit CnTransFilter;
{* |<PRE>
================================================================================
* 软件名称：CnPack 多语包
* 单元名称：多语包 IDE 翻译过滤设置单元
* 单元作者：CnPack开发组 小冬 (kending@21cn.com)
* 备    注：该单元实现了多语包的 IDE 翻译过滤功能
* 开发平台：PWin2000 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2006.10.15 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, {$IFDEF COMPILER6_UP} Variants, {$ENDIF}
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, CheckLst, CnLangUtils;

type
  TFrmTransFilter = class(TForm)
    chklstFilter: TCheckListBox;
    btnCancel: TButton;
    btnOK: TButton;
    lblFilter: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure chklstFilterKeyPress(Sender: TObject; var Key: Char);
  private
    procedure GetFilters(var FilterOptions: TLangTransFilterSet);
    procedure SetFilters(const FilterOptions: TLangTransFilterSet);
  public
    procedure Open(var FilterOptions: TLangTransFilterSet);
  end;

implementation

uses
  CnLangConsts;

{$R *.dfm}

procedure TFrmTransFilter.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Caption := SCnFilterFrmCaption;
  lblFilter.Caption := SCnFilterCaption;
  btnOK.Caption := SCnOKCaption;
  btnCancel.Caption := SCnCancelCaption;

  with chklstFilter do
    for I := 0 to Items.Count - 1 do
      Checked[I] := True;
end;

procedure TFrmTransFilter.GetFilters(var FilterOptions: TLangTransFilterSet);
var
  I: Integer;
begin
  FilterOptions := [];
  with chklstFilter do
    for I := 0 to Items.Count - 1 do
      if Checked[I] then
        Include(FilterOptions, TLangTransFilter(I));
end;

procedure TFrmTransFilter.SetFilters(const FilterOptions: TLangTransFilterSet);
var
  I: Integer;
begin
  with chklstFilter do
    for I := 0 to Items.Count - 1 do
      Checked[I] := TLangTransFilter(I) in FilterOptions;
end;

procedure TFrmTransFilter.Open(var FilterOptions: TLangTransFilterSet);
begin
  if FilterOptions <> [] then
    SetFilters(FilterOptions);

  if ShowModal = mrOk then
    GetFilters(FilterOptions);
end;

procedure TFrmTransFilter.chklstFilterKeyPress(Sender: TObject;
  var Key: Char);
var
  I: Integer;
begin
  if Key = #1 then // Ctrl+A
  begin
    for I := 0 to chklstFilter.Items.Count - 1 do
      chklstFilter.Checked[I] := True;
  end
  else if Key = #4 then // Ctrl+D
  begin
    Key := #0;
    for I := 0 to chklstFilter.Items.Count - 1 do
      chklstFilter.Checked[I] := False;
  end
end;

end.

