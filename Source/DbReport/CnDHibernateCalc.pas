{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2009 CnPack 开发组                       }
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

unit CnDHibernateCalc; 
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate标准控件库
* 单元名称：表达式计算控件单元
* 单元作者：Rarnu (rarnu@cnpack.org)
* 备    注：
* 开发平台：PWinXP SP2 + Delphi 2009
* 兼容测试：Win2000/XP/Vista/2008 + Delphi 2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.08.23 V1.8
*               移植到 Delphi2009
*           2006.09.04 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  SysUtils, Classes, Math, Windows, Messages, Graphics, Controls, Forms, Dialogs, ExtCtrls;

type
  TCnTree = record
    num: integer;
    con: string;
    l, r: pointer;
  end;

  PCnTree = ^TCnTree;

  TCnDHibernateCalc = class
  private
    Err: boolean;
    Bc: integer;
    PrevLex, Curlex: integer;
    Pos: integer;
    FFormula: string;
    Tree: pointer;
    FVariables: TStrings;
    FDefaultNames: boolean;
    procedure init(s: string);
    function geTCnTree(s: string): pointer;
    function deltree(t: PCnTree): pointer;
    procedure Error(s: string);
    procedure SetVariables(Value: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    function calc(args: array of extended): extended;
    property Formula: string read FFormula write init;
    property Variables: TStrings read FVariables write SetVariables;
  end;

  TCnDHibernateCalculator = class(TComponent)
  private
    FFormula: string;
    FCalc: TCnDHibernateCalc;
    FAbout: string;
    function GetVariables: TStrings;
    procedure SetFormula(const Value: string);
    procedure SetVariables(const Value: TStrings);
  protected
  public
    constructor Create(Aowner: TComponent); override;
    destructor Destroy; override;
    function calc(args: array of Extended): Extended;
  published
    property About: string read FAbout write FAbout;
    property Formula: string read FFormula write SetFormula;
    property Variables: TStrings read GetVariables write SetVariables;
  end; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

function TCnDHibernateCalc.calc(args: array of extended): extended;

  function c(t: PCnTree): extended;
  var
    r: extended;
  begin
    c := 0;
    case t^.num of
      3:
        c := c(t^.l) + c(t^.r);
      4:
        c := c(t^.l) - c(t^.r);
      5:
        c := c(t^.l) * c(t^.r);
      6:
        c := c(t^.l) / c(t^.r);
      7:
        c := strtofloat(t^.con);
      8:
        c := args[StrToInt(t^.con)];
      9:
        c := -c(t^.l);
      10:
        c := cos(c(t^.l));
      11:
        c := sin(c(t^.l));
      12:
        c := tan(c(t^.l));
      13:
        c := 1 / tan(c(t^.l));
      14:
        c := abs(c(t^.l));
      15:
        begin
          r := c(t^.l);
          if r < 0 then
            c := -1
          else if r > 0 then
            c := 1
          else
            c := 0;
        end;
      16:
        c := sqrt(c(t^.l));
      17:
        c := ln(c(t^.l));
      18:
        c := exp(c(t^.l));
      19:
        c := arcsin(c(t^.l));
      20:
        c := arccos(c(t^.l));
      21:
        c := arctan(c(t^.l));
      22:
        c := pi / 2 - arctan(c(t^.l));
      23:
        begin
          r := c(t^.l);
          c := (exp(r) - exp(-r)) / 2;
        end;
      24:
        begin
          r := c(t^.l);
          c := (exp(r) + exp(-r)) / 2;
        end;
      25:
        begin
          r := c(t^.l);
          c := (exp(r) - exp(-r)) / (exp(r) + exp(-r));
        end;
      26:
        begin
          r := c(t^.l);
          c := (exp(r) + exp(-r)) / (exp(r) - exp(-r));
        end;
      27:
        begin
          r := c(t^.l);
          if r >= 0 then
            c := 1
          else
            c := 0;
        end;
      31:
        c := power(c(t^.l), c(t^.r));
    end;
  end;

begin
  calc := c(tree);
end;

procedure TCnDHibernateCalc.Error(s: string);
begin
  Err := True;
  raise Exception.Create(s);
end;

constructor TCnDHibernateCalc.Create;
begin
  Tree := nil;
  Formula := '0';
  FDefaultNames := False;
  FVariables := TStringList.Create;
end;

destructor TCnDHibernateCalc.Destroy;
begin
  DelTree(Tree);
  FVariables.Free;
  inherited;
end;

function TCnDHibernateCalc.GeTCnTree(s: string): pointer;

  function getnumber(s: string): string;
  begin
    Result := '';
    try
      while (pos <= length(s)) and ({$IFDEF DELPHI12_UP}CharInSet(s[pos], ['0'..'9']){$ELSE}s[pos] in ['0'..'9']{$ENDIF}) do
      begin
        Result := Result + s[pos];
        inc(pos);
      end;
      if pos > length(s) then
        exit;
      if s[pos] = DecimalSeparator then
      begin
        Result := Result + DecimalSeparator;
        inc(pos);
        if (pos > length(s)) or not ({$IFDEF DELPHI12_UP}CharInSet(s[pos], ['0'..'9']){$ELSE}s[pos] in ['0'..'9']{$ENDIF}) then
          Error('Wrong number.');
        while (pos <= length(s)) and ({$IFDEF DELPHI12_UP}CharInSet(s[pos], ['0'..'9']){$ELSE}s[pos] in ['0'..'9']{$ENDIF}) do
        begin
          Result := Result + s[pos];
          inc(pos);
        end;
      end;
      if pos > length(s) then
        exit;
      if (s[pos] <> 'e') and (s[pos] <> 'E') then
        exit;
      Result := Result + s[pos];
      inc(pos);
      if pos > length(s) then
        Error('Wrong number.');
      if {$IFDEF DELPHI12_UP}CharInSet(s[pos], ['-', '+']){$ELSE}s[pos] in ['-', '+']{$ENDIF} then
      begin
        Result := Result + s[pos];
        inc(pos);
      end;
      if (pos > length(s)) or not ({$IFDEF DELPHI12_UP}CharInSet(s[pos], ['0'..'9']){$ELSE}s[pos] in ['0'..'9']{$ENDIF}) then
        Error('Wrong number.');
      while (pos <= length(s)) and ({$IFDEF DELPHI12_UP}CharInSet(s[pos], ['0'..'9']){$ELSE}s[pos] in ['0'..'9']{$ENDIF}) do
      begin
        Result := Result + s[pos];
        inc(pos);
      end;
    except

    end;
  end;

  procedure getlex(s: string; var num: integer; var con: string);
  begin
    con := '';
    while (pos <= length(s)) and (s[pos] = ' ') do
      inc(pos);
    if pos > length(s) then
    begin
      num := 0;
      exit;
    end;
    case s[pos] of
      '(':
        num := 1;
      ')':
        num := 2;
      '+':
        num := 3;
      '-':
        begin
          num := 4;
          if (pos < length(s))
            and ({$IFDEF DELPHI12_UP}CharInSet(s[pos + 1], ['1'..'9', '0']){$ELSE}s[pos + 1] in ['1'..'9', '0']{$ENDIF})
            and (curlex in [0, 1]) then
          begin
            inc(pos);
            con := '-' + getnumber(s);
            dec(pos);
            num := 7;
          end;
        end;
      '*':
        num := 5;
      '/':
        num := 6;
      '^':
        num := 31;
      'a'..'z', 'A'..'Z', '_':
        begin
          while (pos <= length(s)) and ({$IFDEF DELPHI12_UP}CharInSet(s[pos], ['a'..'z', 'A'..'Z', '_', '1'..'9', '0']){$ELSE}s[pos] in ['a'..'z', 'A'..'Z', '_', '1'..'9', '0']{$ENDIF}) do
          begin
            con := con + s[pos];
            inc(pos);
          end;
          dec(pos);
          num := 8;
          if con = 'cos' then
            num := 10;
          if con = 'sin' then
            num := 11;
          if con = 'tg' then
            num := 12;
          if con = 'ctg' then
            num := 13;
          if con = 'abs' then
            num := 14;
          if (con = 'sgn') or (con = 'sign') then
            num := 15;
          if con = 'sqrt' then
            num := 16;
          if con = 'ln' then
            num := 17;
          if con = 'exp' then
            num := 18;
          if con = 'arcsin' then
            num := 19;
          if con = 'arccos' then
            num := 20;
          if (con = 'arctg') or (con = 'arctan') then
            num := 21;
          if con = 'arcctg' then
            num := 22;
          if (con = 'sh') or (con = 'sinh') then
            num := 23;
          if (con = 'ch') or (con = 'cosh') then
            num := 24;
          if (con = 'th') or (con = 'tanh') then
            num := 25;
          if (con = 'cth') or (con = 'coth') then
            num := 26;
          if (con = 'heaviside') or (con = 'h') then
            num := 27;
          if num = 8 then
            con := IntToStr(FVariables.IndexOf(con));
        end;
      '1'..'9', '0':
        begin
          con := getnumber(s);
          dec(pos);
          num := 7;
        end;
    end;
    inc(pos);
    PrevLex := CurLex;
    CurLex := num;
  end;

var
  neg: boolean;
  l, r, res: PCnTree;
  n, op: integer;
  c: string;

  function newnode: PCnTree;
  begin
    Result := allocmem(sizeof(TCnTree));
    Result^.l := nil;
    Result^.r := nil;
  end;

  function getsingleop: pointer;
  var
    op, bracket: integer;
    opc: string;
    l, r, res: PCnTree;
  begin
    l := nil;
    try
      if n = 1 then
      begin
        inc(bc);
        l := geTCnTree(s);
      end
      else
      begin
        if not (n in [7, 8, 10..30]) then
          Error('');
        op := n;
        opc := c;
        if n in [7, 8] then
        begin
          l := newnode;
          l^.num := op;
          l^.con := opc;
        end
        else
        begin
          getlex(s, n, c);
          if n <> 1 then
            Error('');
          inc(bc);
          l := newnode;
          l^.l := geTCnTree(s);
          l^.num := op;
          l^.con := opc;
        end;
      end;
      getlex(s, n, c);
      while n = 31 do
      begin
        getlex(s, n, c);
        bracket := 0;
        if n = 1 then
        begin
          bracket := 1;
          getlex(s, n, c);
        end;
        if (n <> 7) and (n <> 8) then
          Error('');
        r := newnode;
        r^.num := n;
        r^.con := c;
        res := newnode;
        res^.l := l;
        res^.r := r;
        res^.num := 31;
        l := res;
        if bracket = 1 then
        begin
          getlex(s, n, c);
          if n <> 2 then
            Error('');
        end;
        getlex(s, n, c);
      end;
      Result := l;
    except
      DelTree(l);
      Result := nil;
    end;
  end;

  function getop: pointer;
  var
    op: integer;
    l, r, res: PCnTree;
  begin
    neg := False;
    getlex(s, n, c);
    if prevlex in [0, 1] then
    begin
      if n = 4 then
      begin
        neg := True;
        getlex(s, n, c);
      end;
      if n = 3 then
        getlex(s, n, c);
    end;
    l := getsingleop;
    while n in [5, 6] do
    begin
      op := n;
      getlex(s, n, c);
      r := getsingleop;
      res := allocmem(sizeof(TCnTree));
      res^.l := l;
      res^.r := r;
      res^.num := op;
      l := res;
    end;
    if neg then
    begin
      res := allocmem(sizeof(TCnTree));
      res^.l := l;
      res^.r := nil;
      res^.num := 9;
      l := res;
    end;
    Result := l;
  end;

begin
  l := nil;
  try
    l := getop;
    while True do
    begin
      if n in [0, 2] then
      begin
        if n = 2 then
          dec(bc);
        Result := l;
        exit;
      end;
      if not (n in [3, 4]) then
        Error('');
      op := n;
      r := getop;
      res := allocmem(sizeof(TCnTree));
      res^.l := l;
      res^.r := r;
      res^.num := op;
      l := res;
    end;
    Result := l;
  except
    DelTree(l);
    Result := nil;
  end;
end; 

//******************************************************************

procedure TCnDHibernateCalc.init(s: string);
begin
  deltree(tree);
  Err := False;
  FFormula := LowerCase(s);
  Prevlex := 0;
  Curlex := 0;
  Pos := 1;
  bc := 0;
  Tree := GeTCnTree(Lowercase(s));
  if (bc <> 0) or Err then
  begin
    raise Exception.Create('Error(s) in Expression!');
    Tree := DelTree(Tree);
  end;
end; 

//Tree deletion

function TCnDHibernateCalc.deltree(t: PCnTree): pointer;
begin
  Result := nil;
  if t = nil then
    exit;
  if t^.l <> nil then
    Deltree(t^.l);
  if t^.r <> nil then
    Deltree(t^.r);
  freemem(t);
end;

procedure TCnDHibernateCalc.SetVariables(Value: TStrings);
begin
  FVariables.Clear;
  FVariables.Assign(Value);
  Init(Formula);
end; 

{ TCnDHibernateCalculator }

function TCnDHibernateCalculator.calc(args: array of Extended): Extended;
begin
  Result := FCalc.calc(args);
end;

constructor TCnDHibernateCalculator.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  FCalc := TCnDHibernateCalc.Create;
end;

destructor TCnDHibernateCalculator.Destroy;
begin
  FCalc.Free;
  inherited;
end;

function TCnDHibernateCalculator.GetVariables: TStrings;
begin
  Result := FCalc.FVariables;
end;

procedure TCnDHibernateCalculator.SetFormula(const Value: string);
begin
  FFormula := Value;
  FCalc.Formula := FFormula;
end;

procedure TCnDHibernateCalculator.SetVariables(const Value: TStrings);
begin
  FCalc.FVariables.Assign(Value);
end; 

{$ENDIF SUPPORT_ADO}
end.
