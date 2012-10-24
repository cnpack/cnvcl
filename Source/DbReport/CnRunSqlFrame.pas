{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2012 CnPack 开发组                       }
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

unit CnRunSqlFrame;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：查询分析器组件界面 Frame 实现单元
* 单元作者：不得闲 (appleak46@yahoo.com.cn)
* 备    注：
* 开发平台：PWinXP + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2007.11.24 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
{$IFDEF COMPILER6_UP}
  Variants,
{$ENDIF}
  ImgList, StdCtrls,ExtCtrls, ADODB,CnDBConsts, StdActns, ActnList,Clipbrd,
  Menus, DB, CnDataGrid, ComCtrls, ToolWin, Dialogs, CnRunSqlUnit;

type
  TRunSqlEvent = procedure(Sender: TObject; DsList: TList;
    MsgList: TStrings) of object;
  
  TCnFrameRunSql = class(TFrame)
    ImageList1: TImageList;
    ActionList1: TActionList;
    ActOpenSql: TAction;
    ActBuildSql: TAction;
    ActSaveSql: TAction;
    ActFindSql: TAction;
    Splitter1: TSplitter;
    Panel1: TPanel;
    FindDialog1: TFindDialog;
    ReplaceDialog1: TReplaceDialog;
    ActReplace: TAction;
    PopupMenu1: TPopupMenu;
    FindMenu: TMenuItem;
    ReplaceMenu: TMenuItem;
    ActRun: TAction;
    ToolBar1: TToolBar;
    BtnOpen: TToolButton;
    BtnSave: TToolButton;
    BtnFindReplace: TToolButton;
    ToolButton4: TToolButton;
    BtnUndo: TToolButton;
    BtnRedo: TToolButton;
    ToolButton7: TToolButton;
    BtnCut: TToolButton;
    BtnCopy: TToolButton;
    BtnPaste: TToolButton;
    ToolButton11: TToolButton;
    BtnRun: TToolButton;
    BtnParase: TToolButton;
    BtnStop: TToolButton;
    ToolButton15: TToolButton;
    BtnShowGrid: TToolButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ScrollBox1: TScrollBox;
    MsgEdit: TMemo;
    pnl1: TPanel;
    CodeEdit: TMemo;
    PopupMenu2: TPopupMenu;
    CopyMenu: TMenuItem;
    procedure ActOpenSqlExecute(Sender: TObject);
    procedure ActSaveSqlExecute(Sender: TObject);
    procedure ActFindSqlExecute(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure ReplaceDialog1Replace(Sender: TObject);
    procedure ActReplaceExecute(Sender: TObject);
    procedure CodeEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FindDialog1Close(Sender: TObject);
    procedure ReplaceDialog1Close(Sender: TObject);
    procedure CodeEditChange(Sender: TObject);
    procedure BtnUndoClick(Sender: TObject);
    procedure BtnRedoClick(Sender: TObject);
    procedure BtnCutClick(Sender: TObject);
    procedure BtnCopyClick(Sender: TObject);
    procedure BtnPasteClick(Sender: TObject);
   // procedure BtnUtilitiesClick(Sender: TObject);
    procedure ActRunExecute(Sender: TObject);
    procedure BtnParaseClick(Sender: TObject);
    procedure BtnShowGridClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure ReplaceDialog1Find(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure CopyMenuClick(Sender: TObject);
  private
    { Private declarations }
    OpenFile: string;
    //FindOptions: TSynSearchOptions;
    FindCount: integer;
    KeyFlag,HasRecord,IsParse: boolean;
    CurrentOperate: string;
    LastFontText: string;
    RunThread: TRunThread;
    GridHeight: integer;
    FShowGridPage: boolean;
    List: TStringList;
    procedure ClearRecord;
    function CreateRecord(Ds: TDataSource;isLast: boolean): integer;
    procedure RunEnd(Sender: Tobject);
    procedure RunSql;
    procedure GridKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure SetShowGridPage(const Value: boolean);
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    GridLoading: boolean;
    FindFlag: boolean;
    Connection: TADOConnection;
    Constr: string;
    RunSucc: boolean;
    DataGridFont: TFont;
    CnSQLAnalyzer: TCustomPanel;
    DataSourceList: TList;
    CustomShowGrid: boolean;
    RunEndProc: TRunSqlEvent;
    { Public declarations }
    procedure GridResize(Sender: TObject);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure FreeRes;
    procedure InitRunFrame;
    property  ShowGridPage: boolean read FShowGridPage write SetShowGridPage;
  end;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{$R *.dfm}

function ReverseString(const AText: string): string;
var
  I: Integer;
  P: PChar;
begin
  SetLength(Result, Length(AText));
  P := PChar(Result);
  for I := Length(AText) downto 1 do
  begin
    P^ := AText[I];
    Inc(P);
  end;
end;

{ TCnFrameRunSql }

procedure TCnFrameRunSql.ActOpenSqlExecute(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  begin
    Filter := SCnSqlFilter;
    if Execute then
    begin
      if trim(FileName) <> '' then
      begin
        OpenFile := FileName;
        CodeEdit.Lines.LoadFromFile(FileName);
      end;
    end;
    Free;
  end;
end;

procedure TCnFrameRunSql.ActSaveSqlExecute(Sender: TObject);
begin
  if OpenFile <> '' then
    CodeEdit.Lines.SaveToFile(OpenFile)
  else
  begin
    if Trim(CodeEdit.Text) <> '' then
    begin
      With TSaveDialog.Create(nil) do
      begin
        Filter := SCnSqlFilter;
        if Execute then
        begin
          if Trim(FileName) <> '' then
            CodeEdit.Lines.SaveToFile(FileName);
        end;
        free;
      end;
    end;
  end;
end;

procedure TCnFrameRunSql.WndProc(var Message: TMessage);
begin
  inherited;
  Case Message.Msg of
  WM_CREATE:
     begin
       OpenFile := '';
       FindCount := 0;
       FShowGridPage := false;
       LastFontText := '';
       KeyFlag := false;
       IsParse := false;
       if not Assigned(List) then
         List := TStringList.Create;
       if not Assigned(DataGridFont) then
       begin
         DataGridFont := TFont.Create;
         DataGridFont.Assign(Panel1.Font);
       end;
       if not Assigned(DataSourceList) then
         DataSourceList := TList.Create;
     end;
  CN_MSG_EXEFAIL:
      begin
        //TrunThread(message.WParam).Suspend;
        TrunThread(message.WParam).Terminate;
      end;
  CN_MSG_EXECUTING:
      begin
        ClearRecord;
      end;
  CN_MSG_CLOSEQUERYTOOL:
      FreeRes;
  end;
end;

procedure TCnFrameRunSql.ActFindSqlExecute(Sender: TObject);
begin
  CodeEdit.SelStart := 0;
  FindCount := 0;
  FindDialog1.Execute;
  LastFontText := '';
  CurrentOperate := 'Find';
end;

procedure TCnFrameRunSql.FindDialog1Find(Sender: TObject);
var
  startpos,fPos,flength:integer;
  tempstr: string;
begin
   if frdown in FindDialog1.Options then
   begin
     if CodeEdit.SelLength<>0 then
       startpos:= CodeEdit.SelStart+CodeEdit.SelLength
     else
       startpos:=0;
     tempstr := copy(CodeEdit.Text,startPos + 1,length(CodeEdit.Text)-startpos);
     if not (frMatchCase in FindDialog1.Options) then  //如果不用判断大小写则转换一下
     begin
        tempstr := uppercase(tempstr);
        fpos := Pos(uppercase(FindDialog1.FindText),tempstr);
     end
     else
        fpos := Pos(FindDialog1.FindText,tempstr);
     if fPos <> 0 then
     begin
       CodeEdit.SelStart := fpos + StartPos - 1;
       CodeEdit.SelLength := length(FIndDialog1.FindText);
     end
     else
     begin
       MessageBox(FindDialog1.Handle,PChar(SCnFoundSucced),PChar(SCnMsg),64);
       CodeEdit.SelStart := 0;
     end;
   end
   else
   begin
     tempstr := ReverseString(CodeEdit.Text);
     if CodeEdit.SelLength <> 0 then
       StartPos := CodeEdit.SelStart
     else
       StartPos := length(CodeEdit.Text);
     tempstr := Copy(tempstr,length(tempstr)-startpos + 1,startpos);
     if not (frMatchCase in FindDialog1.Options) then  //如果不用判断大小写则转换一下
     begin
       tempstr := uppercase(tempstr);
       fpos := pos(uppercase(ReverseString(FindDialog1.FindText)),tempstr);
     end
     else
        fpos := Pos(ReverseString(FindDialog1.FindText),tempstr);
     if fPos <> 0 then
     begin
       flength := length(FIndDialog1.FindText);
       CodeEdit.SelStart := StartPos - fPos  - flength + 1;
       CodeEdit.SelLength := flength;
     end
     else
     begin
       MessageBox(FindDialog1.Handle,PChar(SCnFoundSucced),PChar(SCnMsg),64);
       CodeEdit.SelStart := length(CodeEdit.Text);
     end;
   end;
end;

procedure TCnFrameRunSql.ReplaceDialog1Replace(Sender: TObject);
begin
  ReplaceDialog1Find(nil);
  if not(frReplaceAll in ReplaceDialog1.Options) then
  begin
    if FindFlag then
         CodeEdit.SelText := ReplaceDialog1.ReplaceText;
  end
  else
  begin
    while FindFlag do
    begin
      ReplaceDialog1Find(nil);
      if FindFlag then
        CodeEdit.SelText := ReplaceDialog1.ReplaceText;
    end;
  end;
end;

procedure TCnFrameRunSql.ActReplaceExecute(Sender: TObject);
begin
  FindCount := 0;
  ReplaceDialog1.Execute;
  LastFontText := '';
  CurrentOperate := 'Replace';
end;

procedure TCnFrameRunSql.CodeEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F3 then
  begin
    if CurrentOperate = 'Find' then
      FindDialog1Find(nil)
    else if CurrentOperate = 'Replace' then
    begin
      KeyFlag := true;
      ReplaceDialog1Replace(nil);
    end;
  end
  else if Key = VK_F5 then
    RunSql;
  if ssCtrl in Shift then
  begin
    case Key of
    70:
      begin
        FindDialog1.Execute;
        CurrentOperate := 'Find';
      end;
    72:
      begin
        ReplaceDialog1.Execute;
        CurrentOperate := 'Replace';
      end;
    82: BtnShowGrid.Click;
    end;
  end;
end;

procedure TCnFrameRunSql.FindDialog1Close(Sender: TObject);
begin
  CodeEdit.SetFocus;
end;

procedure TCnFrameRunSql.ReplaceDialog1Close(Sender: TObject);
begin
  CodeEdit.SetFocus;
end;

procedure TCnFrameRunSql.CodeEditChange(Sender: TObject);
begin
  if Trim(CodeEdit.Text)='' then
  begin
    //BtnSave.Enabled := false;
    BtnFindReplace.Enabled := false;
    BtnCut.Enabled := false;
    BtnCopy.Enabled := false;
    BtnRun.Enabled := false;
    BtnParase.Enabled := false;
    BtnStop.Enabled := false; 
  end
  else
  begin
    //BtnSave.Enabled := true;
    BtnFindReplace.Enabled := true;
    BtnCut.Enabled := true;
    BtnCopy.Enabled := true;
    BtnRun.Enabled := true;
    BtnParase.Enabled := true;
    BtnStop.Enabled := false;
  end;
end;

procedure TCnFrameRunSql.BtnUndoClick(Sender: TObject);
begin
  CodeEdit.Undo;
end;

procedure TCnFrameRunSql.BtnRedoClick(Sender: TObject);
begin
  CodeEdit.Undo;
end;

procedure TCnFrameRunSql.BtnCutClick(Sender: TObject);
begin
  CodeEdit.CutToClipboard;
end;

procedure TCnFrameRunSql.BtnCopyClick(Sender: TObject);
begin
  CodeEdit.CopyToClipboard;
end;

procedure TCnFrameRunSql.BtnPasteClick(Sender: TObject);
begin
  CodeEdit.PasteFromClipboard;
end;

procedure TCnFrameRunSql.ActRunExecute(Sender: TObject);
begin
   SendMessage(CodeEdit.Handle,WM_KEYDOWN,VK_F5,0);  
end;

procedure TCnFrameRunSql.RunEnd(Sender: Tobject);
var
  Thread: TRunThread;
  i: integer;
  tempstr: pchar;
  Ds: TDataSource;
begin
  Thread := TRunThread(Sender);
  splitter1.Visible := true;
  IsParse := false;
  for i := 0 to Thread.RecordList.Count - 1 do
  begin
    List.Add(inttostr(i));
    GridLoading := true;
    Ds := Thread.RecordList.Items[i];
    DataSourceList.Add(ds);
    if not CustomShowGrid then
    begin
      if i <> Thread.RecordList.Count - 1 then
        GridHeight := CreateRecord(Ds,false)
      else
        GridHeight := CreateRecord(Ds,true);
    end;
  end;
  for i := 0 to Thread.MsgList.Count - 1 do
  begin
     tempstr := Thread.MsgList.Items[i];
     MsgEdit.Lines.Add(string(tempstr));
  end;
  RunSucc := Thread.RunSucced;
  if not CustomShowGrid then
  begin
    Panel1.Visible := true;
    GridLoading := false;
    GridHeight := 0;
    if Thread.RunSucced then
    begin
      if Thread.RecordList.Count > 0 then
        PageControl1.ActivePageIndex := 0
      else
        PageControl1.ActivePageIndex := 1;
    end
    else
      PageControl1.ActivePageIndex := 1;
    Splitter1.Visible := true;
    Splitter1.Top := Panel1.Top - 1;
    BtnParase.Enabled := true;
    BtnStop.Enabled := false;
    ShowGridPage := true;
  end
  else
    Splitter1.Visible := false;
  if Assigned(RunEndProc) then
    RunEndProc(CnSQLAnalyzer,DataSourceList,MsgEdit.Lines);
  Screen.Cursor := 0;
end;

procedure TCnFrameRunSql.ClearRecord;
var
  i: integer;
  Grid: TCnDataGrid;
begin
  for i := ScrollBox1.ControlCount - 1 downto 0 do
  begin
    if SCrollBox1.Controls[i] is TCnDataGrid then
    begin
       Grid := SCrollBox1.Controls[i] as TCnDataGrid;
       Grid.DataSet.Free;
    end;
    ScrollBox1.Controls[i].Free;
  end;
  DataSourceList.Clear;
  List.Clear;
  MsgEdit.Text := '';
end;

procedure TCnFrameRunSql.BtnParaseClick(Sender: TObject);
begin
  IsParse := true;
  ActRunExecute(nil);
end;

function TCnFrameRunSql.CreateRecord(Ds: TDataSource; isLast: boolean): integer;
var
  Grid: TCnDataGrid;
  Split: TSplitter;
begin
  Grid := TCnDataGrid.Create(ScrollBox1);
  Grid.Visible := false;
  Grid.Font.Assign(DataGridFont);
  Grid.Tag := List.Count - 1;
  Grid.Parent := SCrollBox1;
  Grid.OnKeyDown := GridKeyDown;
  grid.flat := true;
  Grid.PopupMenu := PopupMenu2;
  Grid.OnResize := GridResize;
  grid.DataSet := TCustomADoDataSet(Ds.DataSet);

  if Ds.DataSet.RecordCount < 7 then
     Grid.Height := Grid.GridHeight * Ds.DataSet.RecordCount + 5
  else
     Grid.Height := 120;
  result := GridHeight + Grid.Height;
  if isLast then
  begin
    ScrollBox1.VertScrollBar.Range := result + 3;
    Grid.Align := AlClient;
    {if ScrollBox1.VertScrollBar.Range > ScrollBox1.Height then
      Grid.Height := ScrollBox1.VertScrollBar.Range - GridHeight - 5
    else
      Grid.Height := ScrollBox1.Height - GridHeight - 5;}
    Grid.Visible := true;
  end
  else
  begin
    Grid.Visible := true;
    Grid.Top := 100000;
    Grid.Align := AlTop;
    Split := TSplitter.Create(ScrollBox1);
    Split.OnCanResize := Splitter1CanResize;
    split.Top := 100001;
    split.Parent := ScrollBox1;
    split.Align := AlTop;
    split.Color := $00B99D7F;
    split.ResizeStyle := rsLine;

    Split.Height := 2;
  end;
  List.Strings[Grid.Tag] := inttostr(Grid.Height);
end;

procedure TCnFrameRunSql.BtnShowGridClick(Sender: TObject);
begin
  ShowGridPage := not ShowGridPage;
  Panel1.Visible := not Panel1.Visible;
  splitter1.Visible := Panel1.Visible;
end;

procedure TCnFrameRunSql.BtnStopClick(Sender: TObject);
begin
  if not RunThread.IsStop then
  begin
    RunThread.IsStop := true;
    RunThread.Connection.Cancel;
    RunThread.MsgList.Add(strNew(pchar(SCnOperateCancel)));
    RunThread.Terminate;
  end;
end;

procedure TCnFrameRunSql.GridResize(Sender: TObject);
var
  Grid: TCnDataGrid;
  LastHeight: integer;
begin
   if not GridLoading then
   begin
     Grid := TCnDataGrid(Sender);
     LastHeight := strtoint(List.Strings[Grid.Tag]);
     if (Grid.Align <> Alclient) and (Grid.Height <> LastHeight) then
     begin
      ScrollBox1.VertScrollBar.Range := ScrollBox1.Height + Grid.Height - LastHeight
     end;
   end;
end;

procedure TCnFrameRunSql.Splitter1CanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  if NewSize < 33 then
    NewSize := 33;
end;

procedure TCnFrameRunSql.RunSql;
var
  Con: TADOConnection;
begin
   Screen.Cursor := -11;
   BtnParase.Enabled := false;
   BtnStop.Enabled := true;
   RunThread := TRunThread.Create(true,self.Handle);
   HasRecord := false;
   if Connection = nil then
   begin
     if trim(Constr) <> '' then
     begin
       Con := TADOConnection.Create(nil);
       Con.LoginPrompt := False;
       Con.ConnectionString := Constr;
       //Con.ConnectOptions := coAsyncConnect;//使用异步方式查询
       RunThread.Connection := Con;
       RunThread.DBProvider := Con.Provider;
     end
     else
     begin
       Screen.Cursor := 0;
       BtnRun.Enabled := True;
       BtnStop.Enabled := False;
       BtnParase.Enabled := True;
       RunThread.MsgList.Add(strNew(pchar(SCnUnUseConstr)));
       RunThread.Terminate;
       raise Exception.Create(SCnUnUseConstr);
     end;
   end
   else
   begin
     //Connection.ConnectOptions := coAsyncConnect;//使用异步方式查询
     RunThread.Connection := Connection;
     RunThread.DBProvider := COnnection.Provider;
   end;
   RunThread.OnTerminate := RunEnd;
   RunThread.IsParse := IsParse;
   RunThread.FreeOnTerminate := true;
   if CodeEdit.SelLength = 0 then
     RunThread.Sql := CodeEdit.Text
   else
     RunThread.Sql := CodeEdit.SelText;
   RunThread.Resume;
end;

procedure TCnFrameRunSql.ReplaceDialog1Find(Sender: TObject);
var
  startpos, fPos, flength: Integer;
  tempstr: string;
begin
  if frdown in ReplaceDialog1.Options then
  begin
    if CodeEdit.SelLength <> 0 then
      startpos:= CodeEdit.SelStart+CodeEdit.SelLength
    else
      startpos:=0;
    tempstr := copy(CodeEdit.Text,startPos + 1, Length(CodeEdit.Text) - startpos);
    if not (frMatchCase in ReplaceDialog1.Options) then  //如果不用判断大小写则转换一下
    begin
      tempstr := UpperCase(tempstr);
      fpos := Pos(UpperCase(ReplaceDialog1.FindText), tempstr);
    end
    else
      fpos := Pos(ReplaceDialog1.FindText, tempstr);
    if fPos <> 0 then
    begin
      FindFlag := true;
      CodeEdit.SelStart := fpos + StartPos - 1;
      CodeEdit.SelLength := length(ReplaceDialog1.FindText);
    end
    else
    begin
      MessageBox(ReplaceDialog1.Handle,PChar(SCnReplaceSucced),PChar(SCnMsg),64);
      CodeEdit.SelStart := 0;
      FindFlag := false;
    end;
  end
  else
  begin
    tempstr := ReverseString(CodeEdit.Text);
    if CodeEdit.SelLength <> 0 then
      StartPos := CodeEdit.SelStart
    else
      StartPos := length(CodeEdit.Text);
    tempstr := Copy(tempstr,length(tempstr)-startpos + 1,startpos);
    if not (frMatchCase in FindDialog1.Options) then  //如果不用判断大小写则转换一下
    begin
      tempstr := uppercase(tempstr);
      fpos := pos(uppercase(ReverseString(ReplaceDialog1.FindText)),tempstr);
    end
    else
      fpos := Pos(ReverseString(ReplaceDialog1.FindText),tempstr);
    if fPos <> 0 then
    begin
      FindFlag := True;
      flength := length(ReplaceDialog1.FindText);
      CodeEdit.SelStart := StartPos - fPos  - flength + 1;
      CodeEdit.SelLength := flength;
    end
    else
    begin
      FindFlag := False;//没有找到
      MessageBox(ReplaceDialog1.Handle, PChar(SCnReplaceSucced), PChar(SCnMsg), MB_ICONASTERISK);
      CodeEdit.SelStart := Length(CodeEdit.Text);
    end;
  end;
end;

procedure TCnFrameRunSql.FrameResize(Sender: TObject);
begin
  if Panel1.Height > Height - 50 then
    Panel1.Height  := Panel1.Height - 5;
end;

procedure TCnFrameRunSql.FreeRes;
begin
  ClearRecord;
  FreeAndNil(ActionList1);
  FreeAndNil(DataGridFont);
  FreeAndNil(List);
  FreeAndNil(DataSourceList);
  DataSourceList.Free;
  FreeAndNil(ReplaceDialog1);
  FreeAndNil(ImageList1);
  FreeAndNil(FindDialog1);
end;

procedure TCnFrameRunSql.CopyMenuClick(Sender: TObject);
begin
  if PopupMenu2.PopupComponent is TCnDataGrid then
  begin
    ClipBoard.AsText := TCnDataGrid(PopupMenu2.PopupComponent).SelectionText;
  end;
end;

procedure TCnFrameRunSql.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ssCtrl in Shift then
  begin
    if  Key = 67 then
      Clipboard.AsText := TCnDataGrid(Sender).SelectionText;
  end;
end;

procedure TCnFrameRunSql.SetShowGridPage(const Value: boolean);
begin
  FShowGridPage := Value;
  BtnShowGrid.Down := Value;
end;

procedure TCnFrameRunSql.InitRunFrame;
begin
   TabSheet1.Caption := SCnResMsg;
   TabSheet2.Caption := SCnMsg;
   FindMenu.Caption := SCnFindMenu;
   ReplaceMenu.Caption := SCnReplaceMenu;
   CopyMenu.Caption := SCnCopyMenu;

   BtnOpen.Hint := SCnOpenHint;
   BtnSave.Hint := SCnSaveHint;
   BtnFindReplace.Hint := SCnFindHint;
   BtnUndo.Hint := SCnUnDoHint;
   BtnRedo.Hint := SCnReDoHint;
   BtnCut.Hint := SCnCutHint;
   BtnCopy.Hint := SCnCopyHint;
   BtnPaste.Hint := SCnPasteHint;
   BtnRun.Hint := SCnRunHint;
   BtnParase.Hint := SCnParse;
   BtnStop.Hint := SCnStop;
   BtnShowGrid.Hint := SCnShowGrid;

   Font.Size := 9;
   Font.Style := [fsBold];
end;

{$ENDIF SUPPORT_ADO}
end.
