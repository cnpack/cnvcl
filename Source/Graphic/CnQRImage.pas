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

unit CnQRImage;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：二维码显示单元
* 单元作者：CnPack 开发组
* 备    注：本单元使用 CnQRCode 实现了二维码图形绘制。
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2026.01.13 V1.0
*               创建单元，在 AI 帮助下实现编码并能扫描成功
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Graphics, Controls, ExtCtrls, CnQRCode;

type
  TCnQRCodeImage = class(TGraphicControl)
  {* 二维码绘制类}
  private
    FEncoder: TCnQREncoder;
    FIcon: TIcon;
    FCellSize: Integer;
    FForeColor: TColor;
    function GetQRErrorRecoveryLevel: TCnErrorRecoveryLevel;
    procedure SetQRErrorRecoveryLevel(const Value: TCnErrorRecoveryLevel);
    procedure SetText(const Value: string);
    function GetText: string;
    procedure SetIcon(const Value: TIcon);
    procedure SetCellSize(const Value: Integer);
    procedure SetForeColor(const Value: TColor);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveToFile(const FileName: string; Border: Integer = 4);

  published
    property Icon: TIcon read FIcon write SetIcon;
    property QRErrorRecoveryLevel: TCnErrorRecoveryLevel read GetQRErrorRecoveryLevel
      write SetQRErrorRecoveryLevel;

    property Text: string read GetText write SetText;
    {* 显示的字符串}
    property CellSize: Integer read FCellSize write SetCellSize;
    {* 二维码每个模块大小，0 表示自动适应}
    property ForeColor: TColor read FForeColor write SetForeColor default clBlack;
    {* 二维码中的黑色颜色}
  end;

implementation

{ TCnQRCodeImage }

constructor TCnQRCodeImage.Create(AOwner: TComponent);
begin
  inherited;
  FEncoder := TCnQREncoder.Create;
  FIcon := TIcon.Create;
  FForeColor := clBlack;
  Color := clWhite;
end;

destructor TCnQRCodeImage.Destroy;
begin
  FIcon.Free;
  FEncoder.Free;
  inherited;
end;

procedure TCnQRCodeImage.SaveToFile(const FileName: string; Border: Integer = 4);
var
  CS, QRWidth, QRHeight, QRLeft, QRTop, I, J: Integer;
  Edge: Integer;
  Bmp: TBitmap;
  ImgW, ImgH: Integer;
begin
  QRWidth := FEncoder.QRSize + Border * 2;
  QRHeight := FEncoder.QRSize + Border * 2;
  CS := CellSize;
  if CS <= 0 then
  begin
    Edge := Width;
    if Height < Edge then
      Edge := Height;
    CS := Edge div (FEncoder.QRSize + Border * 2);
    if CS <= 0 then CS := 1;
  end;
  ImgW := QRWidth * CS;
  ImgH := QRHeight * CS;
  QRLeft := 0;
  QRTop := 0;
  Bmp := TBitmap.Create;
  try
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := ImgW;
    Bmp.Height := ImgH;
    Bmp.Canvas.Brush.Style := bsSolid;
    Bmp.Canvas.Brush.Color := Color;
    Bmp.Canvas.FillRect(Rect(0, 0, ImgW, ImgH));
    Bmp.Canvas.Brush.Color := ForeColor;
    Bmp.Canvas.Pen.Style := psClear;
    for I := 0 to FEncoder.QRSize - 1 do
    begin
      for J := 0 to FEncoder.QRSize - 1 do
      begin
        if FEncoder.QRData[I, J] = 1 then
          Bmp.Canvas.FillRect(Rect(
            QRLeft + (I + Border) * CS,
            QRTop + (J + Border) * CS,
            QRLeft + (I + Border + 1) * CS,
            QRTop + (J + Border + 1) * CS
          ));
      end;
    end;
    Bmp.SaveToFile(FileName);
  finally
    Bmp.Free;
  end;
end;

function TCnQRCodeImage.GetQRErrorRecoveryLevel: TCnErrorRecoveryLevel;
begin
  Result := FEncoder.QRErrorRecoveryLevel;
end;

function TCnQRCodeImage.GetText: string;
begin
  Result := FEncoder.Text;
end;

procedure TCnQRCodeImage.Paint;
var
  CS, QL, QT, Edge, I, J: Integer;
  QRWidth, QRHeight: Integer;
  QRLeft, QRTop: Integer;
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  if csDesigning in ComponentState then
  begin
    with Canvas do
    begin
      Pen.Style := psSolid;
      Brush.Style := bsSolid;
      Rectangle(0, 0, Width, Height);
    end;
  end;

  // 计算二维码实际绘制区域（包含静区）
  QRWidth := FEncoder.QRSize + 8;  // 4 模块静区 * 2 = 8
  QRHeight := FEncoder.QRSize + 8;

  // 确定绘制位置，画二维码
  CS := CellSize;
  if CS <= 0 then
  begin
    // 根据内容宽高与尺寸宽高，计算合适的 CellSize 以及正方形位置
    Edge := Width;
    if Height < Edge then
      Edge := Height;

    // 静区，按 (QRSize + 8) 计算单元大小
    CS := Edge div (FEncoder.QRSize + 8);
    if CS <= 0 then CS := 1;
  end;

  // 计算绘制位置（居中）
  QRLeft := (Width - QRWidth * CS) div 2;
  QRTop := (Height - QRHeight * CS) div 2;

  // 绘制白色背景（静区）
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(QRLeft, QRTop,
                       QRLeft + QRWidth * CS,
                       QRTop + QRHeight * CS));

  // 绘制二维码模块
  Canvas.Brush.Color := ForeColor;
  Canvas.Pen.Style := psClear;

  for I := 0 to FEncoder.QRSize - 1 do
  begin
    for J := 0 to FEncoder.QRSize - 1 do
    begin
      if FEncoder.QRData[I, J] = 1 then
      begin
        Canvas.FillRect(Rect(
          QRLeft + (I + 4) * CS,      // +4 表示4模块静区
          QRTop + (J + 4) * CS,
          QRLeft + (I + 4 + 1) * CS,
          QRTop + (J + 4 + 1) * CS
        ));
      end;
    end;
  end;

  // 绘制 Icon
  if not FIcon.Empty then
  begin
    QL := (Width - FIcon.Width) div 2;
    QT := (Height - FIcon.Height) div 2;
    Canvas.Draw(QL, QT, FIcon);
  end;
end;

procedure TCnQRCodeImage.SetCellSize(const Value: Integer);
begin
  if FCellSize <> Value then
  begin
    FCellSize := Value;
    Invalidate;
  end;
end;

procedure TCnQRCodeImage.SetForeColor(const Value: TColor);
begin
  if FForeColor <> Value then
  begin
    FForeColor := Value;
    Invalidate;
  end;
end;

procedure TCnQRCodeImage.SetIcon(const Value: TIcon);
begin
  if Value <> nil then
  begin
    FIcon.Assign(Value);
    Invalidate;
  end;
end;

procedure TCnQRCodeImage.SetQRErrorRecoveryLevel(
  const Value: TCnErrorRecoveryLevel);
begin
  if FEncoder.QRErrorRecoveryLevel <> Value then
  begin
    FEncoder.QRErrorRecoveryLevel := Value;
    Invalidate;
  end;
end;

procedure TCnQRCodeImage.SetText(const Value: string);
begin
  if FEncoder.Text <> Value then
  begin
    FEncoder.Text := Value;
    Invalidate;
  end;
end;

end.
