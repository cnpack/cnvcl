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
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnWinampCtrl;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：Winamp控制器组件TCnWinampCtrl单元
* 单元作者：小冬 (kendling@21cn.com)
* 备    注：- 可以控制Winamp的一个小小控件。
*           - 可以用本控件写一个基本上可以完全控制Winamp的软件。
*           - 可以用本控件开发一个歌词编辑器。
* 开发平台：PWin2000 + Delphi 6.0 Update Pack 2
* 兼容测试：PWin2000 + Delphi 6.0 Update Pack 2
* 本 地 化：该单元中没有字符串资源
* 修改记录：
*           2005.03.08 v1.0
*               创建单元
================================================================================
|</PRE>}

interface
    
{$I CnPack.inc}

uses
//------------------------------------------------------------------------------
// System
//------------------------------------------------------------------------------
  SysUtils, Classes, Messages, Windows, ShellAPI,
//------------------------------------------------------------------------------
// CnPack
//------------------------------------------------------------------------------
  CnCommon, CnClasses, CnConsts, CnCompConsts;

const
  WinampClassName = 'Winamp v1.x';

//------------------------------------------------------------------------------
// WA IPC
//------------------------------------------------------------------------------
  WM_WA_IPC             = WM_USER;
  IPC_GETVERSION        = 0;    
  IPC_PLAYFILE          = 100;    
  IPC_ENQUEUEFILE       = 100;
  IPC_DELETE            = 101;
  IPC_CHDIR             = 103;
  IPC_ISPLAYING         = 104;
  IPC_GETOUTPUTTIME     = 105; 
  IPC_JUMPTOTIME        = 106;
  IPC_WRITEPLAYLIST     = 120;
  IPC_SETPLAYLISTPOS    = 121;
  IPC_SETVOLUME         = 122; 
  IPC_SETPANNING        = 123; 
  IPC_GETLISTLENGTH     = 124;
  IPC_GETLISTPOS        = 125; 
  IPC_GETINFO           = 126;  
  IPC_GETEQDATA         = 127;
  IPC_SETEQDATA         = 128;   
  IPC_RESTARTWINAMP     = 135;
  IPC_CHANGECURRENTFILE = 245;
  IPC_GET_SHUFFLE       = 250;
  IPC_GET_REPEAT        = 251;
  IPC_SET_SHUFFLE       = 252;
  IPC_SET_REPEAT        = 253; 
  IPC_ENABLEDISABLE_ALL_WINDOWS = 259;
  IPC_GETWND            = 260;
    IPC_GETWND_EQ       = 0;
    IPC_GETWND_PE       = 1;
    IPC_GETWND_MB       = 2;
    IPC_GETWND_VIDEO    = 3;

  WINAMP_FILE_QUIT      = 40001;
  WINAMP_OPTIONS_EQ     = 40036;
  WINAMP_OPTIONS_PLEDIT = 40040;
  WINAMP_BUTTON1        = 40044;
  WINAMP_BUTTON2        = 40045;
  WINAMP_BUTTON3        = 40046;
  WINAMP_BUTTON4        = 40047;
  WINAMP_BUTTON5        = 40048;
  WINAMP_VOLUMEUP       = 40058;
  WINAMP_VOLUMEDOWN     = 40059;
  WINAMP_FFWD5S         = 40060;
  WINAMP_REW5S          = 40061;
  WINAMP_BUTTON1_SHIFT  = 40144;
  WINAMP_BUTTON2_SHIFT  = 40145;
  WINAMP_BUTTON3_SHIFT  = 40146;
  WINAMP_BUTTON4_SHIFT  = 40147;
  WINAMP_BUTTON5_SHIFT  = 40148;
  WINAMP_BUTTON1_CTRL   = 40154;
  WINAMP_BUTTON2_CTRL   = 40155;
  WINAMP_BUTTON3_CTRL   = 40156;
  WINAMP_BUTTON4_CTRL   = 40157;
  WINAMP_BUTTON5_CTRL   = 40158;
  IDC_SORT_FILENAME     = 40166;
  IDC_SORT_FILETITLE    = 40167;
  IDC_SORT_ENTIREFILENAME = 40168;     
  WINAMP_JUMP10FWD      = 40195;
  WINAMP_JUMP10BACK     = 40197; 
  WINAMP_MAIN_WINDOW    = 40258; 
  WINAMP_MINIMIZE       = 40334;
  
//------------------------------------------------------------------------------
// WA PE
//------------------------------------------------------------------------------   

type
  TEQDataSelect = (EQ60hz, EQ170hz, EQ310hz, EQ600hz, EQ1k, EQ3k,EQ6k, EQ12k,
    EQ14k, EQ16k, EQPreAmp, EQEnabled, EQAutoLoad);

//==============================================================================
// Winamp控制器对象
//==============================================================================

{ TWinampControl }

  TCnWinampCtrl = class(TCnComponent)
  {* Winamp控制器组件}
  private
    FAutoFind: Boolean;
    FAutoWritePlayList: Boolean;
    FStartDelay: Integer;
    FWAPath: string;
    FWndWinamp: HWND;     
    function GetEQData(const Index: TEQDataSelect): Byte;
    function GetIsFound: Boolean;
    function GetPlayListPos: Integer;
    function GetVolume: Byte;
    function GetVolBalance: Integer;
    function GetWACurrentTime: Integer;
    function GetWARepeat: Boolean;
    function GetWAShufle: Boolean;
    function GetWAState: Integer;
    function SendMessageToWinamp(Msg: Cardinal; wParam: WPARAM; lParam:
            LPARAM): Integer;
    procedure SetEnabledWAWindow(const Value: Boolean);
    procedure SetEQData(const Index: TEQDataSelect; const Value: Byte);
    procedure SetPlayListPos(const Value: Integer);
    procedure SetVolume(const Value: Byte);
    procedure SetVolBalance(const Value: Integer);
    procedure SetWACurrentTime(const Value: Integer);
    procedure SetWARepeat(const Value: Boolean);
    procedure SetWAShufle(const Value: Boolean);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
    procedure AddDir(const strPath: string);
    {* 添加目录到播放列表最后}
    procedure AddFile(const strPath: string);
    {* 添加文件到播放列表最后}
    procedure ClearPlayList;
    {* 清除播放列表}
    procedure CloseWinamp;
    {* 关闭Winamp}
    function FindWinamp: Boolean;
    {* 查找Winamp窗口句柄}
    procedure FFW_5sec;
    {* 向前5秒}
    function GetInfo(const iMode: Integer): Integer;
    {* 获取当前歌曲信息  iMode为 0:采样率 1:比特率 2:通道 3:视频 LOWORD=w HIWORD=h 4:> 65536, string (视频描述)}
    function GetPlayList: string;
    {* 获取Winamp当前播放列表  返回格式为TString.CommaText}
    procedure GetPlayListCount;
    {* 获取播放曲目总数}
    function GetTimeLength: Integer;
    {* 获取当前歌曲总时间}
    function GetVersion: string;
    {* 获取Winamp版本}
    procedure JUMP10BACK;
    {* 向后10秒}
    procedure JUMP10FWD;
    {* 向前10秒}
    procedure NextTack;
    {* 下一首歌曲}
    procedure Pause;
    {* 暂停播放}
    procedure Play;
    {* 开始播放}
    procedure PlayIndex(const Index: Integer);
    {* 播放列表中的一首歌}
    procedure PlayListSortInFileName;
    {* 播放列表以文件名排序}
    procedure PlayListSortInFilePath;
    {* 播放列表以路径和文件名排序}
    procedure PlayListSortInTitle;
    {* 播放列表以标题排序}
    procedure PrevTrack;
    {* 上一首歌曲}
    procedure RestartWinamp;
    {* 重新启动Winamp}
    procedure REW_5sec;
    {* 向后5秒}
    function StartWinamp(const strWAPath: string=''): Boolean;
    {* 启动Winamp}
    procedure Stop;
    {* 停止播放}
    procedure VolumeDown;
    {* 减少一点音量 测试中2.95可用，5.08不可用}
    procedure VolumeUp;
    {* 增加一点音量 测试中2.95可用，5.08不可用}
    procedure WritePlayList;
    {* 保存当前播放列表到Winamp程序的目录}
    property EnabledWAWindow: Boolean write SetEnabledWAWindow;
    {* 开启/屏蔽Winamp所有窗口，对新面板无效。}
    property EQData[const Index: TEQDataSelect]: Byte read GetEQData write
        SetEQData; default;
    {* EQ数据 Index为: EQPreAmp, EQ60hz, EQ170hz, EQ310hz, EQ600hz, EQ1k, EQ3k,
    EQ6k, EQ12k, EQ14k, EQ16k, EQEnabled, EQAutoLoad  EQPreAmp..EQ16k:0-63
    EQEnabled/EQAutoLoad为0时关闭，非0时开启。}
    property IsFound: Boolean read GetIsFound default False;
    {* 当前是否已经找到Winamp窗口句柄}
    property PlayListPos: Integer read GetPlayListPos write SetPlayListPos;
    {* 播放列表所选的位置}    
    property Volume: Byte read GetVolume write SetVolume;
    {* Winamp音量 0 - 255}
    property VolBalance: Integer read GetVolBalance write SetVolBalance;
    {* 音量平行 -127 - 127}
    property WACurrentTime: Integer read GetWACurrentTime write SetWACurrentTime;
    {* 歌曲播放的当前时间 单位ms} 
    property WARepeat: Boolean read GetWARepeat write SetWARepeat;
    {* 获取/设置循环播放}
    property WAShufle: Boolean read GetWAShufle write SetWAShufle;
    {* 获取/设置随机播放}
    property WAState: Integer read GetWAState;
    {* 获取Winamp当前状态  返回值：0 为停止  1 为正在播放  3 为暂停}
  published
    property AutoFind: Boolean read FAutoFind write FAutoFind default False;
    {* 自动查找Winamp窗口句柄}
    property AutoWritePlayList: Boolean read FAutoWritePlayList write
        FAutoWritePlayList default False;
    {* 自动保存播放列表}
    property StartDelay: Integer read FStartDelay write FStartDelay default 3000;
    {* 等待Winamp启动的延时}
    property WAPath: string read FWAPath write FWAPath;
    {* Winamp程序路径 如: C:\Program Files\Winamp\Winamp.exe}
  end;

implementation
          
//==============================================================================
// Winamp控制器对象
//==============================================================================

constructor TCnWinampCtrl.Create(AOwner: TComponent);
begin
  inherited;
  FStartDelay := 3000;
  if FAutoFind then FindWinamp;
end;

destructor TCnWinampCtrl.Destroy;
begin
  inherited;
end;

procedure TCnWinampCtrl.AddDir(const strPath: string);
var
  PPath: PChar;
  cds : COPYDATASTRUCT;
begin                
  PPath := PChar(strPath);
  cds.dwData := IPC_CHDIR;
  cds.lpData := PPath;
  cds.cbData := SysUtils.StrLen(PAnsiChar(cds.lpData))+1; // include space for null char
  SendMessageToWinamp(WM_COPYDATA, WPARAM(0), LPARAM(@cds));
  if FAutoFind then WritePlayList;
end;

procedure TCnWinampCtrl.AddFile(const strPath: string);
var
  PPath: PChar;
  cds : COPYDATASTRUCT;
begin
  PPath := PChar(strPath);
  cds.dwData := IPC_PLAYFILE;
  cds.lpData := PPath;
  cds.cbData := SysUtils.StrLen(PAnsiChar(cds.lpData))+1; // include space for null char
  SendMessageToWinamp(WM_COPYDATA, WPARAM(0), LPARAM(@cds));
  if FAutoFind then WritePlayList;
end;

procedure TCnWinampCtrl.ClearPlayList;
begin
  SendMessageToWinamp(WM_WA_IPC, 0, IPC_DELETE);
  if FAutoFind then WritePlayList;
end;

procedure TCnWinampCtrl.CloseWinamp;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_FILE_QUIT ,0);
  FWndWinamp := 0;
end;

function TCnWinampCtrl.FindWinamp: Boolean;
begin
  FWndWinamp := FindWindow(WinampClassName, nil);
  Result := IsFound;
end;

procedure TCnWinampCtrl.FFW_5sec;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_FFWD5S, 0);
end;

procedure TCnWinampCtrl.GetComponentInfo(var AName, Author, Email, Comment:
    string);
begin
  AName := SCnWinampCtrlName;
  Author := SCnPack_Kendling;
  Email := SCnPack_KendlingEmail;
  Comment := SCnWinampCtrlComment;
end;

function TCnWinampCtrl.GetEQData(const Index: TEQDataSelect): Byte;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, Ord(Index), IPC_GETEQDATA);
end;

function TCnWinampCtrl.GetInfo(const iMode: Integer): Integer;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, iMode, IPC_GETINFO);
end;

function TCnWinampCtrl.GetPlayList: string;
var
  slPlayList, slCPlayList: TStrings;
  i, j: Integer;
begin
  {检测是否设置了Winamp路径}
  Result := '';
  if not FileExists(FWAPath) then Exit;
  if FAutoFind then WritePlayList;
  {获取当前播放列表}
  slPlayList := TStringList.Create;
  slCPlayList := TStringList.Create;
  slPlayList.LoadFromFile(_CnChangeFileExt(FWAPath, '.m3u'));
  if UpperCase(slPlayList[0]) = '#EXTM3U' then
  begin
    i := 1;
    while i < slPlayList.Count do
    begin
      if UpperCase(Copy(slPlayList[i], 1, 7)) = '#EXTINF' then
      begin
        j := AnsiPos(',', slPlayList[i]);
        slCPlayList.Add(Copy(slPlayList[i], j+1, 256));
        Inc(i);
      end else
      begin
        slCPlayList.Add(_CnExtractFileName(slPlayList[i]));
      end;
      Inc(i);
    end;
  {输出播放列表}
    Result := slCPlayList.CommaText;
  end;
  slPlayList.Free;
  slCPlayList.Free;
end;

function TCnWinampCtrl.GetIsFound: Boolean;
begin
  Result := FWndWinamp <> 0; //INVALID_HANDLE_VALUE [DWord(-1)]; 2005.3.7 QQCAT
end;

procedure TCnWinampCtrl.GetPlayListCount;
begin
  SendMessageToWinamp(WM_WA_IPC, 0, IPC_GETLISTLENGTH);
end;

function TCnWinampCtrl.GetPlayListPos: Integer;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, 0, IPC_GETLISTPOS);
end;

function TCnWinampCtrl.GetTimeLength: Integer;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, 1, IPC_GETOUTPUTTIME);
end;

function TCnWinampCtrl.GetVersion: string;
begin
  Result := IntToHex(SendMessageToWinamp(WM_WA_IPC, 0, IPC_GETVERSION), 2);
  if Result = '00' then
  begin
    Result := '0';
    Exit;
  end;
  if Result[1] = '2' then Result[3] := Result[2];
  if Result[1] = '1' then Result[3] := Result[2];
    Result[2] := '.';
end;

function TCnWinampCtrl.GetVolume: Byte;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, WPARAM(-666), IPC_SETVOLUME);
end;

function TCnWinampCtrl.GetVolBalance: Integer;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, WPARAM(-666), IPC_SETPANNING);
end;

function TCnWinampCtrl.GetWACurrentTime: Integer;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, 0, IPC_GETOUTPUTTIME);
end;

function TCnWinampCtrl.GetWARepeat: Boolean;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, 0, IPC_GET_REPEAT)>0;
end;

function TCnWinampCtrl.GetWAShufle: Boolean;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, 0, IPC_GET_SHUFFLE)>0;
end;

function TCnWinampCtrl.GetWAState: Integer;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, 0, IPC_ISPLAYING);
end;

procedure TCnWinampCtrl.JUMP10BACK;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_JUMP10BACK, 0);
end;

procedure TCnWinampCtrl.JUMP10FWD;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_JUMP10FWD, 0);
end;

procedure TCnWinampCtrl.NextTack;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_BUTTON5, 0);
end;

procedure TCnWinampCtrl.Pause;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_BUTTON3, 0);
end;

procedure TCnWinampCtrl.Play;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_BUTTON2, 0);
end;

procedure TCnWinampCtrl.PlayIndex(const Index: Integer);
begin
  SendMessageToWinamp(WM_WA_IPC, WPARAM(Index), IPC_CHANGECURRENTFILE);
end;

procedure TCnWinampCtrl.PlayListSortInFileName;
begin
  SendMessageToWinamp(WM_COMMAND, IDC_SORT_FILENAME, 0);
end;

procedure TCnWinampCtrl.PlayListSortInFilePath;
begin
  SendMessageToWinamp(WM_COMMAND, IDC_SORT_ENTIREFILENAME, 0);
end;

procedure TCnWinampCtrl.PlayListSortInTitle;
begin
  SendMessageToWinamp(WM_COMMAND, IDC_SORT_FILETITLE, 0);
end;

procedure TCnWinampCtrl.PrevTrack;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_BUTTON1, 0);
end;

procedure TCnWinampCtrl.RestartWinamp;
begin
  SendMessageToWinamp(WM_WA_IPC, 0, IPC_RESTARTWINAMP);
end;

procedure TCnWinampCtrl.REW_5sec;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_REW5S, 0);
end;

function TCnWinampCtrl.SendMessageToWinamp(Msg: Cardinal; wParam: WPARAM;
        lParam: LPARAM): Integer;
begin
  Result := 0;
  if not IsFound then Exit;
  Result := SendMessage(FWndWinamp, Msg, wParam, lParam);
end;

procedure TCnWinampCtrl.SetEnabledWAWindow(const Value: Boolean);
begin
  if Value then
    SendMessageToWinamp(WM_WA_IPC, 0, IPC_ENABLEDISABLE_ALL_WINDOWS)
  else
    SendMessageToWinamp(WM_WA_IPC, WPARAM($DEADBEEF), IPC_ENABLEDISABLE_ALL_WINDOWS);
end;

procedure TCnWinampCtrl.SetEQData(const Index: TEQDataSelect; const Value:
    Byte);
begin
  SendMessageToWinamp(WM_WA_IPC, Ord(Index), IPC_GETEQDATA);
  SendMessageToWinamp(WM_WA_IPC, Value, IPC_SETEQDATA);
end;

procedure TCnWinampCtrl.SetPlayListPos(const Value: Integer);
begin
  SendMessageToWinamp(WM_WA_IPC, Value, IPC_SETPLAYLISTPOS)
end;

procedure TCnWinampCtrl.SetVolume(const Value: Byte);
begin
  SendMessageToWinamp(WM_WA_IPC, Value, IPC_SETVOLUME);
end;

procedure TCnWinampCtrl.SetVolBalance(const Value: Integer);
begin
  SendMessageToWinamp(WM_WA_IPC, Value, IPC_SETPANNING);
end;

procedure TCnWinampCtrl.SetWACurrentTime(const Value: Integer);
begin
  SendMessageToWinamp(WM_WA_IPC, Value, IPC_JUMPTOTIME);
end;

procedure TCnWinampCtrl.SetWARepeat(const Value: Boolean);
begin
  SendMessageToWinamp(WM_WA_IPC, Integer(Value), IPC_SET_REPEAT);
end;

procedure TCnWinampCtrl.SetWAShufle(const Value: Boolean);
begin
  SendMessageToWinamp(WM_WA_IPC, Integer(Value), IPC_SET_SHUFFLE);
end;

function TCnWinampCtrl.StartWinamp(const strWAPath: string=''): Boolean;
begin
  Result := False;
  if strWAPath <> '' then FWAPath := strWAPath;
  if not FileExists(FWAPath) then Exit;
  if ShellExecute(0, '', PChar(FWAPath), PChar('/CLASS="'+WinAmpClassName+'"'),
    PChar(_CnExtractFilePath(FWAPath)), SW_MINIMIZE) > 32 then
    Result := True;
  if FAutoFind then
  begin
    Sleep(FStartDelay);
    FindWinamp;
  end;
end;

procedure TCnWinampCtrl.Stop;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_BUTTON4, 0);
end;

procedure TCnWinampCtrl.VolumeDown;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_VOLUMEDOWN, 0);
end;

procedure TCnWinampCtrl.VolumeUp;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_VOLUMEUP, 0);
end;

procedure TCnWinampCtrl.WritePlayList;
begin
  SendMessageToWinamp(WM_WA_IPC, 0, IPC_WRITEPLAYLIST);
end;

end.
