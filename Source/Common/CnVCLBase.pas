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

unit CnVCLBase;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：基本类定义单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：该单元定义了组件包的基础类库
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2002.04.08 V1.0
*               整理单元，更新版本号
*           2002.02.01 V0.02Demo
*               测试代码、修正已知错误
*               优化CopyParentImage方法（针对TCnGraphicControl进行快速显示）
*               增加Alpha属性允许指定控件透明度（快速刷新）
*               增加大量注释
*           2002.01.11 V0.01Demo
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, CnGraphics,
  CnClasses, CnConsts;

type

//------------------------------------------------------------------------------
// 图形控件基础类
//------------------------------------------------------------------------------

{ TCnGraphicControl }

  TCnGraphicControl = class(TControl)
  {* CnPack 界面类图形控件基础类}
  private
    FLoaded: Boolean;
    FUpdateCount: Integer;
    FIsChanged: Boolean;
    FFace: TCnBitmap;
    FTransparent: Boolean;
    FAlphaBlend: Boolean;
    FAlphaBlendValue: TCnAlpha;
    FFullPaint: Boolean;
    FPainting: Boolean;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure SetAlphaBlendValue(const Value: TCnAlpha);
    procedure DoPaint;
    procedure DoChanged;
    procedure SetFullPaint(const Value: Boolean);
    procedure SetAlphaBlend(const Value: Boolean);
  protected
    function IsUpdating: Boolean;
    {* 当前更新计数是否大于 0（正在更新），只读属性}
    procedure WndProc(var Msg: TMessage); override;
    procedure Paint; virtual;
    {* 控件绘制方法，当控件当前状态改变后自动调用该方法，供重载用
     |<BR> 控件重载此方法对 Face 进行绘制}
    procedure Loaded; override;
    {* 运行期属性已装载（或设计期第一次 Paint 时调用），可重载
     |<BR> 重载时请调用 inherited}
    procedure Changed; virtual;
    {* 属性已变更，请在修改控件属性后调用该方法，以通知控件重绘，可重载
     |<BR> 调用该方法后不需要再调用控件的 Invalidate 等方法
     |<BR> 重载时请调用 inherited}
    procedure OnFaceChange(Sender: TObject); virtual;
    procedure OnChildChange(Sender: TObject); virtual;
    {* 子属性已变更通知，默认为调用 Changed 方法，可重载}
    procedure MouseEnter; virtual;
    {* 鼠标移入控件通知，可重载，重载时请调用 inherited 以产生事件}
    procedure MouseLeave; virtual;
    {* 鼠标移出控件通知，可重载，重载时请调用 inherited 以产生事件}
    procedure SetTransparent(const Value: Boolean); virtual;
    {* 设置控件透明属性，可重载，重载时请调用 inherited}
    property Face: TCnBitmap read FFace;
    {* 控件表面画布镜象，TCnBitmap 类型，在内存中保存了控件当前图像
     |<BR> 当需要在屏幕上绘制控件时，直接将该位图复制到屏幕上以获得快速的显示
     |<BR> 该属性类似于其它控件的 Canvas 属性，用户可直接在 Face 上绘图，如果需要
           在屏幕上输出，请调用控件的Refresh或Repaint方法}
    property AlphaBlend: Boolean read FAlphaBlend write SetAlphaBlend default False;
    {* 控件的半透明属性，如果为真允许控件显示为半透明效果，不透明度为 AlphaBlendValue}
    property AlphaBlendValue: TCnAlpha read FAlphaBlendValue write SetAlphaBlendValue
      default csMaxAlpha;
    {* 控件的不透明度属性，受 AlphaBlend 的影响，为0时完全透明，255时不透明}
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    {* 控件的背景透明属性，为真允许控件显示为不规则的形状，即背景透明}
    property FullPaint: Boolean read FFullPaint write SetFullPaint default True;
    {* 决定当设置 Transparent 属性时，是否每次重绘控件都复制控件背景
    |<BR> 当控件后面的控件和父控件画面为静态时，关闭该属性可提高显示速度
    |<BR> 当控件后面的控件和父控件画面为动态时，请设置为真}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    {* 鼠标移入控件事件}
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    {* 鼠标移出控件事件}
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    {* 控件画布重绘事件}
  public
    constructor Create(AOwner: TComponent); override;
    {* 构造器，创建一个控件实例}
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    {* 设置控件的位置和大小，可重载}
    procedure BeginUpdate;
    {* 开始更新控件，更新过程中控件不重绘，可重载
     |<BR> 在对成批属性进行修改时请调用该方法，注意必须与 EndUpdate 成对使用}
    procedure EndUpdate;
    {* 结束更新，如果当前更新计数为0，自动调用 Change 方法，可重载
     |<BR> 在对成批属性修改后请调用该方法，注意必须与 BeginUpdate 成对使用}
    procedure Repaint; override;
    {* 强迫控件在屏幕上绘制，如果手动修改了 Face 属性，请调用此方法，可重载}
    procedure ReDraw; virtual;
    {* 强迫控件重新绘制自身，并在屏幕上输出，可重载}
  end;

//------------------------------------------------------------------------------
// 窗口控件基础类
//------------------------------------------------------------------------------

{ TCnWinControl }

  TCnWinControl = class(TWinControl)
  private
    FLoaded: Boolean;
    FUpdateCount: Integer;
    FIsChanged: Boolean;
    FFace: TCnBitmap;
    FTransparent: Boolean;
    FAlphaBlendValue: TCnAlpha;
    FFullPaint: Boolean;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FAlphaBlend: Boolean;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure SetAlphaBlendValue(const Value: TCnAlpha);
    procedure DoPaint;
    procedure DoChanged;
    procedure SetFullPaint(const Value: Boolean);
    procedure SetAlphaBlend(const Value: Boolean);
  protected
    function IsUpdating: Boolean;
    procedure WndProc(var Msg: TMessage); override;
    procedure Paint; virtual;
    procedure Loaded; override;
    procedure Changed; virtual;
    procedure OnFaceChange(Sender: TObject); virtual;
    procedure OnChildChange(Sender: TObject); virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure SetTransparent(const Value: Boolean); virtual;
    procedure PaintControls(Bmp: TCnBitmap; First: TControl);
    procedure PaintHandler(var message: TWMPaint);
    property Face: TCnBitmap read FFace;
    property AlphaBlend: Boolean read FAlphaBlend write SetAlphaBlend default False;
    property AlphaBlendValue: TCnAlpha read FAlphaBlendValue write SetAlphaBlendValue
      default csMaxAlpha;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property FullPaint: Boolean read FFullPaint write SetFullPaint default True;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Repaint; override;
  end;

implementation

type
  TParentControl = class(TWinControl);
  TCnPersistentAccess = class(TCnPersistent);
  TCnFontAccess = class(TCnFont);

// 从父控件复制图像
// 代码修改自 RxLibrary VCLUtils
procedure CopyParentImage(Control: TControl; Bmp: TCnBitmap);
var
  I, Count, x, y, SaveIndex: Integer;
  r, SelfR, CtlR: TRect;
  Parent: TWinControl;
  // CnParent: TCnWinControl;
begin
  if (Control = nil) or (Control.Parent = nil) then Exit;
  Count := Control.Parent.ControlCount;
  Parent := Control.Parent;
  {if Control.Parent is TCnWinControl then
  begin
    CnParent := TCnWinControl(Control.Parent);
    Bmp.DrawEx(0, 0, CnParent.Face, Control.ClientRect);
  end;}
  with Parent do
    ControlState := ControlState + [csPaintCopy];
  try
    with Control do
    begin
      SelfR := Bounds(Left, Top, Width, Height);
      x := -Left;
      y := -Top;
    end;
    // 复制父控件图像
    SaveIndex := SaveDC(Bmp.DC);
    try
      SetViewportOrgEx(Bmp.DC, x, y, nil);
      IntersectClipRect(Bmp.DC, 0, 0, Parent.ClientWidth,
        Parent.ClientHeight);
      try
        with TParentControl(Parent) do
        begin
          Perform(WM_ERASEBKGND, Bmp.DC, 0);
          PaintWindow(Bmp.DC);
        end;
      except
        ;
      end;
    finally
      RestoreDC(Bmp.DC, SaveIndex);
    end;
    // 复制图形控件图像
    for I := 0 to Count - 1 do // 按 Z-Order 顺序
    begin
      if Parent.Controls[I] = Control then // 只考虑在后面的控件
        Break
      else if (Parent.Controls[I] <> nil) then
      begin
        if (Parent.Controls[I] is TCnGraphicControl) then
          with TCnGraphicControl(Parent.Controls[I]) do
          begin               // TCnGraphicControl 控件直接从 Face 中复制（快速）
            CtlR := Bounds(Left, Top, Width, Height);
            if IntersectRect(r, SelfR, CtlR) and (Visible or (csDesigning in
              ComponentState)) then // 范围相交
              Bmp.AlphaDraw(Left - Control.Left, Top - Control.Top, Face, ClientRect,
                AlphaBlendValue);
          end
        else if not (Parent.Controls[I] is TWinControl) then // 其它的图形控件
          with Parent.Controls[I] do
          begin
            CtlR := Bounds(Left, Top, Width, Height);
            if IntersectRect(r, SelfR, CtlR) and (Visible or (csDesigning in
              ComponentState)) then
            begin
              ControlState := ControlState + [csPaintCopy];
              SaveIndex := SaveDC(Bmp.DC);
              try
                SaveIndex := SaveDC(Bmp.DC);
                SetViewportOrgEx(Bmp.DC, Left + x, Top + y, nil);
                IntersectClipRect(Bmp.DC, 0, 0, Width, Height);
                Perform(WM_PAINT, Bmp.DC, 0); // 强制控件绘制到目标 DC
              finally
                RestoreDC(Bmp.DC, SaveIndex);
                ControlState := ControlState - [csPaintCopy];
              end;
            end;
          end;
      end;
    end;
  finally
    with Parent do
      ControlState := ControlState - [csPaintCopy];
  end;
end;

//------------------------------------------------------------------------------
// 图形控件基础类
//------------------------------------------------------------------------------

{ TCnGraphicControl }

// 初始化
constructor TCnGraphicControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque]; // 禁止刷新时擦除背景
  FUpdateCount := 0;
  FLoaded := False;
  FFace := TCnBitmap.Create(OnFaceChange);
  FFace.Transparent := False;
  FFace.GdiAllocStyle := gsNormal;
  TCnFontAccess(FFace.Font).Owner := Self;
  TCnPersistentAccess(FFace.Font.Gradient).Owner := FFace.Font;
  FTransparent := False;
  FFullPaint := True;
  FAlphaBlend := False;
  FAlphaBlendValue := csMaxAlpha; // 不透明
  FPainting := False;
end;

// 释放
destructor TCnGraphicControl.Destroy;
begin
  FFace.Free;
  inherited;
end;

//--------------------------------------------------------//
// 可重载的方法                                           //
//--------------------------------------------------------//

// 运行期属性已装载（或设计期第一次 Paint 时调用）
// 重载时请调用 inherited
procedure TCnGraphicControl.Loaded;
begin
  inherited;
  FLoaded := True;
  Changed;
end;

// 属性已变更
procedure TCnGraphicControl.Changed;
begin
  if IsUpdating then
    FIsChanged := True
  else
    DoChanged;
end;

// 控件绘制方法
// 控件在该过程中对 Face 进行绘制
procedure TCnGraphicControl.Paint;
begin

end;

// 鼠标移入
procedure TCnGraphicControl.MouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

// 鼠标移出
procedure TCnGraphicControl.MouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

// 画布内容变更
procedure TCnGraphicControl.OnFaceChange(Sender: TObject);
begin
  //
end;

// 子属性变更
procedure TCnGraphicControl.OnChildChange(Sender: TObject);
begin
  Changed;
end;

//--------------------------------------------------------//
// 辅助功能方法                                           //
//--------------------------------------------------------//

// 开始更新（更新期间不刷新显示）
procedure TCnGraphicControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

// 结束更新
procedure TCnGraphicControl.EndUpdate;
begin
  Assert(FUpdateCount > 0, 'Unpaired TCnGraphicControl.EndUpdate');
  Dec(FUpdateCount);

  if not IsUpdating and FIsChanged then // 属性已变更
    DoChanged;
end;

// 正在更新
function TCnGraphicControl.IsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

// 属性已变更，刷新控件
procedure TCnGraphicControl.DoChanged;
begin
  FIsChanged := False;
  DoPaint;
  Invalidate;
end;

// 刷新控件显示
procedure TCnGraphicControl.DoPaint;
begin
  if not FPainting then
  begin
    FPainting := True;
    if FTransparent then
      CopyParentImage(Self, Face) // 如果透明先复制父控件图像
    else
      Face.Fill(Color);       // 不透明用 Color 填充
    Paint;                    // 调用虚拟方法Paint绘制控件
    FPainting := False;
  end;
end;

// 控件刷新显示方法
procedure TCnGraphicControl.Repaint;
begin
  inherited;
  { TODO -o周劲羽 -c图形控件基础类 : 控件重绘方法 }
end;

// 控件重新绘制画布
procedure TCnGraphicControl.ReDraw;
begin
  Changed;
end;

// 控件重绘消息
procedure TCnGraphicControl.WMPaint(var Msg: TWMPaint);
var
  Bmp: TCnBitmap;
begin
  if (csDesigning in ComponentState) and not FLoaded then
  begin
    FLoaded := True;          // 设计期第一次重绘调用 Loaded 方法
    Loaded;
  end;

  if AlphaBlend and (AlphaBlendValue = 0) then Exit; // 完全透明

  if FTransparent and FFullPaint then
    DoPaint;                  // 透明且完整刷新时每次重绘都更新

  if not AlphaBlend or (AlphaBlendValue = csMaxAlpha) then // 无半透明效果
  begin
    if Msg.DC <> 0 then
    begin
      Face.Lock;
      try
        Face.DrawTo(Msg.DC, 0, 0); // 画布绘制到目标DC
      finally
        Face.Unlock;
      end;
    end;
  end
  else
  begin                       // 部分透明效果
    Bmp := TCnBitmap.Create;
    try
      Bmp.LoadBlank(Width, Height);
      CopyParentImage(Self, Bmp); // 从父控件复制图像
      Bmp.AlphaDraw(Face, AlphaBlendValue, False); // Alpha混合
      Bmp.DrawTo(Msg.DC, 0, 0); // 绘制到目标DC
    finally
      Bmp.Free;
    end;
  end;
end;

// 消息处理过程
procedure TCnGraphicControl.WndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    CM_COLORCHANGED, CM_TEXTCHANGED, CM_FONTCHANGED: Changed;
    CM_MOUSEENTER: MouseEnter; // 鼠标移入
    CM_MOUSELEAVE: MouseLeave; // 鼠标移出
  end;
  inherited;
end;

// 设置控件边界
procedure TCnGraphicControl.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  if (Face.Width <> Width) or (Face.Height <> Height) then
  begin
    Face.LoadBlank(Width, Height); // 修改画布尺寸
    Changed;
  end;
end;

// 设置透明
procedure TCnGraphicControl.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

// 设置不透明度
procedure TCnGraphicControl.SetAlphaBlendValue(const Value: TCnAlpha);
begin
  if FAlphaBlendValue <> Value then
  begin
    FAlphaBlendValue := Value;
    Changed;
  end;
end;

// 设置不透明度支持
procedure TCnGraphicControl.SetAlphaBlend(const Value: Boolean);
begin
  if FAlphaBlend <> Value then
  begin
    FAlphaBlend := Value;
    Changed;
  end;
end;

// 设置透明时完整重绘
procedure TCnGraphicControl.SetFullPaint(const Value: Boolean);
begin
  if FFullPaint <> Value then
  begin
    FFullPaint := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------
// 窗口控件基础类
//------------------------------------------------------------------------------

{ TCnWinControl }

// 初始化
constructor TCnWinControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque]; // 禁止刷新时擦除背景
  FUpdateCount := 0;
  FLoaded := False;
  FFace := TCnBitmap.Create(OnFaceChange);
  FFace.Transparent := False;
  FFace.GdiAllocStyle := gsNormal;
  FTransparent := False;
  FFullPaint := True;
  FAlphaBlend := False;
  FAlphaBlendValue := csMaxAlpha; // 不透明
end;

// 释放
destructor TCnWinControl.Destroy;
begin
  FFace.Free;
  inherited;
end;

//--------------------------------------------------------//
// 可重载的方法                                           //
//--------------------------------------------------------//

// 运行期属性已装载（或设计期第一次 Paint 时调用）
// 重载时请调用 inherited
procedure TCnWinControl.Loaded;
begin
  inherited;
  FLoaded := True;
  Changed;
end;

// 属性已变更
procedure TCnWinControl.Changed;
begin
  if IsUpdating then
    FIsChanged := True
  else
    DoChanged;
end;

// 控件绘制方法
// 控件在该过程中对 Face 进行绘制
procedure TCnWinControl.Paint;
begin

end;

// 鼠标移入
procedure TCnWinControl.MouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

// 鼠标移出
procedure TCnWinControl.MouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

// 画布内容变更
procedure TCnWinControl.OnFaceChange(Sender: TObject);
begin
  //
end;

// 子属性变更
procedure TCnWinControl.OnChildChange(Sender: TObject);
begin
  Changed;
end;

//--------------------------------------------------------//
// 辅助功能方法                                           //
//--------------------------------------------------------//

// 开始更新（更新期间不刷新显示）
procedure TCnWinControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

// 结束更新
procedure TCnWinControl.EndUpdate;
begin
  Assert(FUpdateCount > 0, 'Unpaired TCnWinControl.EndUpdate');
  Dec(FUpdateCount);

  if not IsUpdating and FIsChanged then // 属性已变更
    DoChanged;
end;

// 正在更新
function TCnWinControl.IsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

// 属性已变更，刷新控件
procedure TCnWinControl.DoChanged;
begin
  FIsChanged := False;
  DoPaint;
  Invalidate;
end;

// 刷新控件显示
procedure TCnWinControl.DoPaint;
begin
  if FTransparent then
    CopyParentImage(Self, Face) // 如果透明先复制父控件图像
  else
    Face.Fill(Color);         // 不透明用 Color 填充
  Paint;                      // 调用虚拟方法 Paint 绘制控件
end;

// 控件重绘方法
procedure TCnWinControl.Repaint;
begin
  inherited;
  { TODO -o周劲羽 -c图形控件基础类 : 控件重绘方法 }
end;

// 控件重绘消息
procedure TCnWinControl.WMPaint(var Msg: TWMPaint);
var
  Bmp: TCnBitmap;
begin
  if (csDesigning in ComponentState) and not FLoaded then
  begin
    FLoaded := True;          // 设计期第一次重绘调用 Loaded 方法
    Loaded;
  end;

  if AlphaBlend and (AlphaBlendValue = 0) then Exit; // 完全透明

  if FTransparent and FFullPaint then
    DoPaint;                  // 透明且完整刷新时每次重绘都更新

  if not AlphaBlend or (AlphaBlendValue = csMaxAlpha) then // 无半透明效果
  begin
    if Msg.DC <> 0 then
    begin
      Face.Lock;
      try
        Face.DrawTo(Msg.DC, 0, 0); // 画布绘制到目标DC
      finally
        Face.Unlock;
      end;
    end;
  end
  else
  begin                       // 部分透明效果
    Bmp := TCnBitmap.Create;
    try
      Bmp.LoadBlank(Width, Height);
      CopyParentImage(Self, Bmp); // 从父控件复制图像
      Bmp.AlphaDraw(Face, AlphaBlendValue, False); // Alpha 混合
      Bmp.DrawTo(Msg.DC, 0, 0); // 绘制到目标DC
    finally
      Bmp.Free;
    end;
  end;
end;

// 消息处理过程
procedure TCnWinControl.WndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    CM_COLORCHANGED, CM_TEXTCHANGED, CM_FONTCHANGED: Changed;
    CM_MOUSEENTER: MouseEnter; // 鼠标移入
    CM_MOUSELEAVE: MouseLeave; // 鼠标移出
  end;
  inherited;
end;

// 设置控件边界
procedure TCnWinControl.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  if (Face.Width <> Width) or (Face.Height <> Height) then
  begin
    Face.LoadBlank(Width, Height); // 修改画布尽寸
    Changed;
  end;
end;

// 设置透明
procedure TCnWinControl.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

// 设置不透明度
procedure TCnWinControl.SetAlphaBlendValue(const Value: TCnAlpha);
begin
  if FAlphaBlendValue <> Value then
  begin
    FAlphaBlendValue := Value;
    Changed;
  end;
end;

// 设置不透明度支持
procedure TCnWinControl.SetAlphaBlend(const Value: Boolean);
begin
  if FAlphaBlend <> Value then
  begin
    FAlphaBlend := Value;
    Changed;
  end;
end;

// 设置透明时完整重绘
procedure TCnWinControl.SetFullPaint(const Value: Boolean);
begin
  if FFullPaint <> Value then
  begin
    FFullPaint := Value;
    Changed;
  end;
end;

procedure TCnWinControl.PaintControls(Bmp: TCnBitmap; First: TControl);
//var
  //I, Count, SaveIndex: Integer;
  //FrameBrush: HBRUSH;
begin
  {if FControls <> nil then
  begin
    I := 0;
    if First <> nil then
    begin
      I := FControls.IndexOf(First);
      if I < 0 then I := 0;
    end;
    Count := FControls.Count;
    while I < Count do
    begin
      with TControl(FControls[I]) do
        if (Visible or (csDesigning in ComponentState) and
          not (csNoDesignVisible in ControlStyle)) and
          RectVisible(DC, Rect(Left, Top, Left + Width, Top + Height)) then
        begin
          if csPaintCopy in Self.ControlState then
            Include(FControlState, csPaintCopy);
          SaveIndex := SaveDC(DC);
          MoveWindowOrg(DC, Left, Top);
          IntersectClipRect(DC, 0, 0, Width, Height);
          Perform(WM_PAINT, DC, 0);
          RestoreDC(DC, SaveIndex);
          Exclude(FControlState, csPaintCopy);
        end;
      Inc(I);
    end;
  end;
  if FWinControls <> nil then
    for I := 0 to FWinControls.Count - 1 do
      with TWinControl(FWinControls[I]) do
        if FCtl3D and (csFramed in ControlStyle) and
          (Visible or (csDesigning in ComponentState) and
          not (csNoDesignVisible in ControlStyle)) then
        begin
          FrameBrush := CreateSolidBrush(ColorToRGB(clBtnShadow));
          FrameRect(DC, Rect(Left - 1, Top - 1, Left + Width, Top + Height),
            FrameBrush);
          DeleteObject(FrameBrush);
          FrameBrush := CreateSolidBrush(ColorToRGB(clBtnHighlight));
          FrameRect(DC, Rect(Left, Top, Left + Width + 1, Top + Height + 1),
            FrameBrush);
          DeleteObject(FrameBrush);
        end;}
end;

procedure TCnWinControl.PaintHandler(var message: TWMPaint);
begin

end;

end.

