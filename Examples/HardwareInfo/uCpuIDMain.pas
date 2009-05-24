unit uCpuIDMain;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：TCnCpuId 演示程序
* 单元作者：SkyJacker(HeMiaoYu@gmail.com)
* 备    注：
* 开发平台：WinXP sp2 + Delphi 6.0 up2
* 兼容测试：无
* 本 地 化：该单元中的字符串不符合本地化处理方式
* 单元标识：$Id: uCpuIDMain.pas,v 1.5 2008/08/01 10:48:56 liuxiao Exp $
* 修改记录：2007.01.23
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFrmCPUIDs = class(TForm)
    BtnGetCpuIDs: TButton;
    Memo1: TMemo;
    btnGetUsage: TButton;
    btnGetCpuOems: TButton;
    btnInfoStr: TButton;
    btnGetBios: TButton;
    procedure BtnGetCpuIDsClick(Sender: TObject);
    procedure btnGetUsageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGetCpuOemsClick(Sender: TObject);
    procedure btnInfoStrClick(Sender: TObject);
    procedure btnGetBiosClick(Sender: TObject);
  private
    { Private declarations }
    procedure log(const Info: string);
  public
    { Public declarations }
  end;

var
  FrmCPUIDs: TFrmCPUIDs;

implementation

uses
  CnHardWareInfo;

var
  CnCpuID: TCnCpuID;
  
{$R *.dfm}

procedure TFrmCPUIDs.log(const Info: string);
begin
  memo1.Lines.Add(Info);
end;

procedure TFrmCPUIDs.BtnGetCpuIDsClick(Sender: TObject);
var
  I: Integer;
begin
  for I:=0 to CnCpuID.CPUCount-1 do
    if CnCpuID.SupportCPUId[I] then
      Log('第 '+IntToStr(I)+' 个 CPU 支持 cpuid 指令。')
    else
      Log('第 '+IntToStr(I)+' 个 CPU 不支持 cpuid 指令。');

  Log('');
  for I:=0 to CnCpuID.CPUCount-1 do
    if CnCpuID.SupportCPUSn[I] then
      Log('第 '+IntToStr(I)+' 个 CPU 支持读取序列号。')
    else
      Log('第 '+IntToStr(I)+' 个 CPU 不支持读取序列号。');

  //默认样式
  Log('');
  Log('默认样式');
  Log('CPU的个数：'+IntToStr(CnCpuID.CPUCount));
  Log('单机CPU：'+CnCpuID.FirstCPUId);

  for I:=0 to CnCpuID.CPUCount-1 do
  begin
    Log('第 '+IntToStr(I)+' 个 CPU的序列号：'+CnCpuID.CPUId[I]);
  end;

  //修改显示样式
  Log('');
  Log('分隔符样式');
  CnCpuID.CPUIdFormat := ifDashed;
  Log('CPU的个数：'+IntToStr(CnCpuID.CPUCount));
  Log('单机CPU：'+CnCpuID.FirstCPUId);

  for I:=0 to CnCpuID.CPUCount-1 do
  begin
    Log('第 '+IntToStr(I)+' 个 CPU的序列号：'+CnCpuID.CPUId[I]);
  end;
end;

procedure TFrmCPUIDs.btnGetUsageClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to CnCpuID.CPUCount - 1 do
  begin
    Log('第 '+IntToStr(I)+' 个 CPU的占用率：'+ InttoStr(CnCpuID.CPUUsage[I]));
  end;
  Log('CPU平均占用率：'+ InttoStr(CnCpuID.AverageCPUUsage));
  Log('');
end;

procedure TFrmCPUIDs.FormCreate(Sender: TObject);
begin
  CnCpuID := TCnCpuId.Create;
end;

procedure TFrmCPUIDs.FormDestroy(Sender: TObject);
begin
  CnCpuID.Free;
end;

procedure TFrmCPUIDs.btnGetCpuOemsClick(Sender: TObject);
var
  I: Integer;
begin
  Log('');
  for I := 0 to CnCpuID.CPUCount-1 do
    Log('第 '+IntToStr(I)+' 个 CPU的生产厂商：'+CnCpuID.CPUOem[I]);
  Log('');
end;

procedure TFrmCPUIDs.btnInfoStrClick(Sender: TObject);
var
  I: Integer;
begin
  //修改显示样式
  Log('');
  Log('分隔符样式');
  CnCpuID.CPUIdFormat := ifDashed;
  Log('CPU的个数：'+IntToStr(CnCpuID.CPUCount));
  Log('单机CPU：'+CnCpuID.FirstCPUInfoString);

  for I:=0 to CnCpuID.CPUCount-1 do
  begin
    Log('第 '+IntToStr(I)+' 个 CPU的信息串：'+CnCpuID.CPUInfoString[I]);
  end;
end;

procedure TFrmCPUIDs.btnGetBiosClick(Sender: TObject);
begin
  Log(CnGetBiosID);
end;

end.
