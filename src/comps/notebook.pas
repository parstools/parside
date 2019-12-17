unit notebook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Contnrs, Controls, LMessages, ComCtrls, ExtCtrls,
  LCLType, LCLIntf;

type
  TEdNotebook = class;

  TCloseEnum = (clClose, clCloseAllSave, clCloseAllDiscard, clNo, clCancel, clError);
  TConsiderEnum = (coCanClose, coCanSave, coSaveAs);

  TBeforeCloseQuery = procedure(Control: TEdNotebook; Index: Integer; var Consider: TConsiderEnum) of object;
  TCloseQueryEvent = procedure(Control: TEdNotebook; Index: Integer; var CanClose: TCloseEnum) of object;
  TCloseEvent = procedure(Control: TEdNotebook; Index: Integer) of object;
  TDrawTabEvent = procedure(Control: TEdNotebook; Index: Integer; IsActive: Boolean; ACanvas: TCanvas; var R: TRect; var DefaultDraw: boolean) of object;

  { TEdSheet }

  TEdTabs = class;

  TEdSheet = class(TWinControl)
  private
    FCaption: string;
    FIntfPtr: IUnknown;
    FTabs: TEdTabs;
    procedure SetCaption(AValue: string);
    procedure SetIntfPtr(AValue: IUnknown);
  protected
  public
    FocusedControl: TWinControl;
    constructor Create(AOwner: TComponent); override;
    property Caption: string read FCaption write SetCaption;
    property IntfPtr: IUnknown read FIntfPtr write SetIntfPtr;
    procedure MakeActive();
    procedure TryClose();
    procedure DrawTabs;
  end;

  { TEdTabs }

  TEdTabs = class(TGraphicControl)
  private
    CTextGap: integer;
    CAutoRepeatSpeed:integer;
    CAutoRepeatDelay:integer;
    FSmallButSize: integer;
    CButSize: integer;
    FFirstVisible: integer;
    FMovedStart: integer;
    FMovedRel: integer;
    FMovedCur: integer;
    FIsPressedMouse: boolean;
    FIsPressedTab: boolean;
    FIsMoved: boolean;
    FTabWidths: TList;
    FPressedCloseButton: integer;
    FPressedLeft, FPressedRight: boolean;
    FPressedPlus: boolean;
    FPageIndex: integer;
    FToSwap: integer;
    PrevMouse: integer;
    PrevDirection: integer;
    FTimer: TTimer;
    FTimerButton: integer;
    function FindHighestAbsolute(PageIndex: integer; BoundR: integer): integer;
    function FindLowesttAbsolute(PageIndex: integer; BoundL: integer): integer;
    function FindToSwap(PageIndex: integer): integer;
    function DrawOrHit(bDraw: boolean; X, Y: integer; var APart: integer): integer;
    function AbsoluteStartPosition(PageIndex: integer): integer;
    procedure resetTimer;
    function StartPosition(PageIndex: integer): integer;
  protected
    procedure Paint; override;
    procedure OnTimer(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SwapTabs(m, n: integer);
    procedure Delete(Index: integer);
    //@todo add
    function DetermineWidth(ACaption: string): integer;
    procedure UpdateSheet(Index: integer; ACaption: string);
  end;

  { TEdNotebook }

  TEdNotebook = class(TWinControl)
  private
    FSheets: TObjectList;
    FTabPosition: TTabPosition;
    FTabHeight: integer;
    FTabs: TEdTabs;
    FTextColorActive: TColor;
    FTextColorInactive: TColor;
    FOnBeforeCloseQuery: TBeforeCloseQuery;
    FOnCloseQuery: TCloseQueryEvent;
    FOnClose: TCloseEvent;
    FOnDrawTab: TDrawTabEvent;
    procedure DrawHelper(Index: Integer; IsActive: Boolean; ACanvas: TCanvas; var R: TRect; var DefaultDraw: boolean);
    procedure ActivateCurrent;
    procedure FastCloseTab(Index: integer);
    procedure CloseTab(Index: integer);
    function GetActiveSheet: TEdSheet;
    function GetPage(Index: integer): TEdSheet;
    function GetPageCount: integer;
    function GetPageIndex: integer;
    function GetTabHeight: integer;
    function GetTabPosition: TTabPosition;
    procedure SetActiveSheet(AValue: TEdSheet);
    procedure SetPageIndex(AValue: integer);
    procedure SetTabHeight(AValue: integer);
    procedure SetTabPosition(AValue: TTabPosition);
    procedure SetTextColorActive(AValue: TColor);
    procedure SetTextColorInactive(AValue: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateSheet(sheet: TEdSheet);
    function AddTabSheet(ACaption: string=''): TEdSheet;
    procedure SwapTabs(m, n: integer);
    function GetSheetIndex(sheet: TEdSheet): Integer;
    procedure TryCloseTab(Index: integer; var CanClose: TCloseEnum);
    procedure TryCloseSheet(sheet: TEdSheet; var CanClose: TCloseEnum);
    function TryCloseAll: TCloseEnum;
    property Page[Index: integer]: TEdSheet read GetPage;
    property TabPosition: TTabPosition read GetTabPosition write SetTabPosition default tpTop;
    property TextColorActive: TColor read FTextColorActive write SetTextColorActive;
    property TextColorInactive: TColor read FTextColorInactive write SetTextColorInactive;
    property ActiveSheet: TEdSheet read GetActiveSheet write SetActiveSheet;
    procedure WMTryCloseSheet(var Msg: TLMNotify); message WM_USER+1000;
    procedure WMTryCloseTab(var Msg: TLMNotify); message WM_USER+1001;
    procedure ActivePrev();
    procedure ActiveNext();
  published
    property PageCount: integer read GetPageCount default 0;
    property PageIndex: integer read GetPageIndex write SetPageIndex default -1;
    property TabHeight: integer read GetTabHeight write SetTabHeight default 21;
    property OnBeforeCloseQuery: TBeforeCloseQuery read FOnBeforeCloseQuery write FOnBeforeCloseQuery;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
    property OnClose: TCloseEvent read FOnClose write FOnClose;
    property OnDrawTab: TDrawTabEvent read FOnDrawTab write FOnDrawTab;
  end;

implementation

uses
  Types, Math, Dialogs;

{ TEdTabs }

function TEdTabs.FindHighestAbsolute(PageIndex: integer; BoundR: integer): integer;
var
  Sibling, pos: integer;
begin
  if PageIndex>=FTabWidths.Count-1 then
  begin
    Result:=PageIndex;
    exit;
  end;
  pos := AbsoluteStartPosition(FPageIndex + 1);
  Sibling := PageIndex + 1;
  begin
    if BoundR > pos + max(PtrInt(FTabWidths[Sibling]) - PtrInt(FTabWidths[FPageIndex]), PtrInt(FTabWidths[Sibling]) div 2) then
    begin
      Result := Sibling;
      exit;
    end;
    Inc(pos, ptrInt(FTabWidths[Sibling]));
  end;
  Result := PageIndex;
end;

function TEdTabs.FindLowesttAbsolute(PageIndex: integer; BoundL: integer): integer;
var
  Sibling, pos: integer;
begin
  if PageIndex<=0 then
  begin
    Result:=PageIndex;
    exit;
  end;
  pos := 0;
  for Sibling := 0 to FPageIndex - 1 do
  begin
    if BoundL < pos + min(ptrInt(FTabWidths[Sibling]) div 2, ptrInt(FTabWidths[FPageIndex])) then
    begin
      Result := Sibling;
      exit;
    end;
    Inc(pos, ptrInt(FTabWidths[Sibling]));
  end;
  Result:=FPageIndex;
end;

function TEdTabs.FindToSwap(PageIndex: integer): integer;
var
  FirstVisPos: integer;
begin
  FirstVisPos := AbsoluteStartPosition(FFirstVisible);
  if FMovedCur > FMovedStart then
    Result := FindHighestAbsolute(FPageIndex, FMovedCur + ptrInt(FTabWidths[FPageIndex]) + FirstVisPos)
  else
    Result := FindLowesttAbsolute(FPageIndex, FMovedCur + FirstVisPos);
end;

function TEdTabs.DrawOrHit(bDraw: boolean; X, Y: integer; var APart: integer): integer;
type
  TArrowDirection = (adLeft, adRight);
var
  i, pos: integer;
  R, RButt, RPlus: TRect;
  RLeft, RRight: TRect;
  Notebook: TEdNotebook;

  procedure DrawArrow(R: TRect; Pressed,Enabled: boolean; Direction: TArrowDirection);
  var
    h: integer; //triangle height, width=2*h-1
    px, py: integer;
    Color: TColor;
  begin
    if not bDraw then
      exit;
    h := 3;
    px := (R.Left + R.Right) div 2 - 1;
    py := (R.Top + R.Bottom + h) div 2 - 1;
    if Pressed then
    begin
      Inc(px);
      Inc(py);
    end;
    if Enabled then Color:=clBlack else Color:=clGray;
    Canvas.Pen.Color := Color;
    Canvas.Brush.Color := Color;
    if Direction = adLeft then
      Canvas.Polygon([Point(px - 1, py), Point(px + h - 1, py + h), Point(px + h - 1, py - h)])
    else
      Canvas.Polygon([Point(px + 1, py), Point(px - h + 1, py + h), Point(px - h + 1, py - h)]);
  end;

  procedure DrawButton(R: TRect; Pressed: boolean);
  begin
    if Pressed then
    begin
       Canvas.Pen.Color := TColor($d78318);
       Canvas.Brush.Color := TColor($fce4cc);
    end
    else
    begin
       Canvas.Pen.Color := TColor($acacac);
       Canvas.Brush.Color := clCream;
    end;
    Canvas.Rectangle(R);
  end;

var
  ACanvas: TCanvas;
  DefaultDraw: Boolean;
begin
  if bDraw then
    ACanvas := Canvas
  else
    ACanvas := nil;
  Result := -1;
  APart := -1;
  Notebook := owner as TEdNotebook;

  pos := 0;
  for i := FFirstVisible to FTabWidths.Count - 1 do
  begin
    R.Top := 0;
    R.Bottom := Height;
    R.Left := pos;
    R.Right := pos + ptrInt(FTabWidths[i]);
    R.Right := min(R.Right, Width);
    R.Bottom := min(R.Bottom, Height);

    if i = FPageIndex then
    begin
      if bDraw then
      begin
        if FIsMoved then
          ACanvas.Brush.Color := TColor($808080)
        else
          ACanvas.Brush.Color := clWhite;
      end;
      Dec(R.Bottom, 2);
    end
    else
    begin
      if bDraw then
        ACanvas.Brush.Color := TColor($E0F0FF);
      Inc(R.Top, 2);
    end;
    if bDraw then
    begin
      ACanvas.Font.Color:=Font.Color;
      DefaultDraw := true;
      Notebook.DrawHelper(i,i=FPageIndex,ACanvas,R,DefaultDraw);
      if DefaultDraw then
      begin
        ACanvas.FillRect(R);
        ACanvas.Line(R.Right - 1, 2, R.Right - 1, R.Bottom);
        if i <> FPageIndex then
        begin
          if Notebook.TabPosition=tpBottom then
            ACanvas.Line(R.Left, R.Top, R.Right, R.Top)
          else
            ACanvas.Line(R.Left, R.Bottom - 1, R.Right, R.Bottom - 1);
        end;
        if (i <> FPageIndex) or  not FIsMoved then
        begin
          if i = FPageIndex then ACanvas.Font.Quality:=fqDefault else ACanvas.Font.Quality:=fqNonAntialiased;
          ACanvas.TextRect(R, R.Left + 7, R.Top + 2, (Notebook.FSheets[i] as TEdSheet).Caption);
        end;
      end;
    end;

    RButt.Left := R.Right - FSmallButSize - CTextGap;
    RButt.Right := R.Right - CTextGap;
    RButt.Top := R.Top;
    RButt.Bottom := R.Top + FSmallButSize;
    if (i <> FPageIndex) or not FIsMoved then
      if bDraw then
      begin
        DrawButton(RButt, FPressedCloseButton=i);
        if i = FPageIndex then
           ACanvas.Pen.Color := TColor($A00000)
        else
           ACanvas.Pen.Color := clBlack;
        ACanvas.Line(RButt.Left + 3, RButt.Top + 3, RButt.Right - 3, RButt.Bottom - 3);
        ACanvas.Line(RButt.Left + 3, RButt.Bottom - 4, RButt.Right - 3, RButt.Top + 2);
        ACanvas.Pen.Color := clBlack;
      end;

    if (X > R.Left) and (X < R.Right) and (Y > R.Top) and (Y < R.Bottom) then
    begin
      Result := i;
      if (X > RButt.Left) and (X < RButt.Right) and (Y > RButt.Top) and (Y < RButt.Bottom) then
        APart := 1
      else
        APArt := 0;
    end;

    Inc(pos, ptrInt(FTabWidths[i]));
    if pos > Width then
      break;
  end;

  Canvas.Brush.Color:=clWhite;

  RPlus.Left := pos + 4;
  RPlus.Bottom := Height - 2;
  RPlus.Top := RPlus.Bottom - CButSize;
  RPlus.Right := RPlus.Left + CButSize;

  if (X > RPlus.Left) and (X < RPlus.Right) and (Y > RPlus.Top) and (Y < RPlus.Bottom) then
  begin
    APArt := 2;
    Result := 3;
    exit;
  end;

  if bDraw then
  begin
    DrawButton(RPlus, FPressedPlus);
    ACanvas.Pen.Color := clBlack;
    ACanvas.Line(RPlus.Left + 3, (RPlus.Top + RPlus.Bottom) div 2 - 1, RPlus.Right - 3, (RPlus.Top + RPlus.Bottom) div 2 - 1);
    ACanvas.Line((RPlus.Left + RPlus.Right) div 2 - 1, RPlus.Top + 3, (RPlus.Left + RPlus.Right) div 2 - 1, RPlus.Bottom - 3);
  end;

  RRight.Right := Width;
  RRight.Bottom := Height - 2;
  RRight.Left := RRight.Right - CButSize;
  RRight.Top := RRight.Bottom - CButSize;
  if (X > RRight.Left) and (X < RRight.Right) and (Y > RRight.Top) and (Y < RRight.Bottom) then
  begin
    APArt := 2;
    Result := 1;
    exit;
  end;


  RLeft := RRight;
  RLeft.Left := RRight.Left - CButSize;
  RLeft.Right := RRight.Right - CButSize;
  if (X > RLeft.Left) and (X < RLeft.Right) and (Y > RLeft.Top) and (Y < RLeft.Bottom) then
  begin
    APArt := 2;
    Result := 2;
    exit;
  end;

  if bDraw then
  begin
    DrawButton(RLeft, FPressedLeft);
    DrawButton(RRight, FPressedRight);
    DrawArrow(RLeft, FPressedLeft, FFirstVisible>0, adLeft);
    DrawArrow(RRight, FPressedRight, FFirstVisible<FTabWidths.Count-1, adRight);
    Canvas.Pen.Color := clBlack;
  end;

  if FIsMoved then
  begin
    R.Left := FMovedCur;
    R.Top := 0;
    R.Right := FMovedCur + ptrInt(FTabWidths[FPageIndex]);
    R.Bottom := Height;
    if bDraw then
    begin
      ACanvas.Brush.Color := clWhite;
      ACanvas.FillRect(R);
      ACanvas.TextRect(R, R.Left + CTextGap, R.Top + 2, (Notebook.FSheets[FPageIndex] as TEdSheet).Caption);
    end;
  end;
end;

function TEdTabs.AbsoluteStartPosition(PageIndex: integer): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to PageIndex - 1 do
  begin
    Inc(Result, ptrInt(FTabWidths[i]));
  end;
end;

function TEdTabs.StartPosition(PageIndex: integer): integer;
begin
  Result := AbsoluteStartPosition(PageIndex) - AbsoluteStartPosition(FFirstVisible);
end;

procedure TEdTabs.Paint;
var
  Part: integer;
begin
  DrawOrHit(True, 0, 0, Part);
end;

procedure TEdTabs.resetTimer;
begin
  FTimer.Enabled:=false;
  FTimerButton:=0;
  FTimer.Interval:=CAutoRepeatDelay;
end;

procedure TEdTabs.OnTimer(Sender: TObject);
begin
  FTimer.Interval:=CAutoRepeatSpeed;
  if FTimerButton=1 then
  begin
    if FFirstVisible<FTabWidths.Count-1 then
      inc(FFirstVisible)
    else resetTimer;
  end else
  if FTimerButton=2 then
  begin
    if FFirstVisible>0 then
      dec(FFirstVisible)
    else resetTimer;
  end;
  Invalidate;
end;

procedure TEdTabs.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Butt, Part: integer;
  Notebook: TEdNotebook;
begin
  Notebook := owner as TEdNotebook;
  Butt := DrawOrHit(False, X, Y, Part);
  inherited MouseDown(Button, Shift, X, Y);
  FIsPressedMouse := True;
  if Butt >= 0 then
  begin
    if Part = 0 then
    begin
      Notebook.PageIndex := Butt;
      FMovedStart := StartPosition(Notebook.PageIndex);
      FMovedRel := X - FMovedStart;
      FMovedCur := FMovedStart;
      PrevMouse:=X;
      PrevDirection:=0;
      FIsPressedTab := True;
    end else if Part = 1 then
    begin
      FPressedCloseButton := Butt;
    end else if Part = 2 then
    begin
        if Butt=1 then
        begin
          if FFirstVisible<FTabWidths.Count-1 then
          begin
            FPressedRight:=true;
            inc(FFirstVisible);
            FTimerButton:=1;
            FTimer.Enabled:=true;
          end;
        end else
        if Butt=2 then
        begin
          if FFirstVisible>0 then
          begin
            FPressedLeft:=true;
            dec(FFirstVisible);
            FTimerButton:=2;
            FTimer.Enabled:=true;
          end;
        end else if Butt=3 then
        begin
          FPressedPlus:=true;
        end;
    end;
    Invalidate;
  end;
end;

procedure TEdTabs.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  resetTimer;
  FToSwap := -1;
  FIsPressedMouse := False;
  FIsPressedTab := False;
  FIsMoved := False;
  FPressedLeft:=false;
  FPressedRight:=false;
  FPressedPlus:=false;
  if (FPressedCloseButton>=0) then
    PostMessage((Owner as TWinControl).Handle, WM_USER+1001, 0, LParam(FPressedCloseButton));
  FPressedCloseButton := -1;
  Invalidate;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TEdTabs.MouseMove(Shift: TShiftState; X, Y: integer);
var Direction: integer;
  procedure UpDown;
  begin
      PrevDirection:=Direction;
      FMovedStart := StartPosition(FPageIndex);
      FMovedRel := X - FMovedStart;
      FMovedCur := FMovedStart;
      exit;
  end;
var
  Butt,Part: integer;
begin
  inherited MouseMove(Shift, X, Y);
  if FIsPressedMouse then
  begin
    if FPressedCloseButton>=0 then
    begin
      Butt := DrawOrHit(False, X, Y, Part);
      if Butt<>FPressedCloseButton then
      begin
        FPressedCloseButton := -1;  //go away tab
        Invalidate;
      end;
    end;
    {Tab swapping}
    if not FIsPressedTab then exit;
    if (FMovedRel<0) or (FMovedRel>ptrInt(FTabWidths[FPageIndex])) then exit;
    if (FFirstVisible>0) and (X<=0) then
    begin
      dec(FFirstVisible);
      UpDown;
    end;
    if (FFirstVisible<FTabWidths.Count-1) and (X>=Width-2*CButSize) then
    begin
      inc(FFirstVisible);
      UpDown;
    end;
    if X=PrevMouse then Direction:=0
    else if X>PrevMouse then Direction:=1
    else Direction:=-1;
    if (FPageIndex<=0)and(Direction=-1) then UpDown;
    if (FPageIndex>=FTabWidths.Count-1)and(Direction=1) then UpDown;
    PrevMouse:=X;
    if (Direction<>0) and (Direction=-PrevDirection) then UpDown;
    PrevDirection:=Direction;
    FIsMoved := True;
    FMovedCur := X - FMovedRel;
    FToSwap := FindToSwap(FPageIndex);
    if (FToSwap <> FPageIndex) then
    begin
     (Owner as TEdNotebook).SwapTabs(FPageIndex, FToSwap);
    end
    else
      Invalidate;
  end;
end;

constructor TEdTabs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CAutoRepeatSpeed:=175;
  CAutoRepeatDelay:=250;
  FTabWidths := TList.Create;
  FPressedCloseButton := -1;
  FPressedLeft:=false;
  FPressedRight:=false;
  FPressedPlus:=false;
  CTextGap := 7;
  CButSize := 16;
  FSmallButSize := 12;
  FToSwap := -1;
  FTimer := TTimer.Create(self);
  FTimer.Enabled := false;
  FTimer.OnTimer := @OnTimer;
  FTimer.Interval := CAutoRepeatDelay;
end;

destructor TEdTabs.Destroy;
begin
  FTabWidths.Free;
  inherited Destroy;
end;

procedure TEdTabs.SwapTabs(m, n: integer);
begin
  FTabWidths.Exchange(m,n);
end;

procedure TEdTabs.Delete(Index: integer);
begin
  FTabWidths.Delete(Index);
  if FPageIndex>Index then
     dec(FPageIndex);
  FPageIndex:=min(FPageIndex, FTabWidths.Count-1);
  FFirstVisible := max(min(FFirstVisible, FTabWidths.Count-1),0);
end;

function TEdTabs.DetermineWidth(ACaption: string): integer;
var
  size: TSize;
begin
  size := Canvas.TextExtent(ACaption);
  Result := size.Width + 2 * CTextGap + FSmallButSize;
  if FSmallButSize > 0 then
    Inc(Result, CTExtGap);
end;

procedure TEdTabs.UpdateSheet(Index: integer; ACaption: string);
begin
  FTabWidths[Index] := pointer(DetermineWidth(ACaption));
  Invalidate;
end;

{ TMySheet }

procedure TEdSheet.SetCaption(AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  (Owner as TEdNotebook).UpdateSheet(self);
end;

procedure TEdSheet.SetIntfPtr(AValue: IUnknown);
begin
  if FIntfPtr=AValue then Exit;
  FIntfPtr:=AValue;
end;

constructor TEdSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIntfPtr := nil;
  FTabs := (Owner as TEdNotebook).FTabs;
end;

procedure TEdSheet.MakeActive();
begin
  (Owner as TEdNotebook).ActiveSheet := self;
end;

procedure TEdSheet.TryClose();
begin
  PostMessage((Owner as TWinControl).Handle, WM_USER+1000, 0, LParam(self));
end;

procedure TEdSheet.DrawTabs;
var Part: integer;
begin
  FTabs.DrawOrHit(True, 0, 0, Part);
end;

constructor TEdNotebook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Left := 0;
  Top := 0;
  Width := 200;
  Height := 100;
  FSheets := TObjectList.Create(True);
  Font.Size := 10;
  Font.Quality := fqNonAntialiased;
  FTabPosition := tpTop;
  FTabHeight := 21;
  FTabs := TEdTabs.Create(Self);
  InsertControl(FTabs);
  FTabs.Width := Width;
  FTabs.Height := FTabHeight;
  FTabs.Align := alTop;
  FTabs.FPageIndex := -1;
end;

destructor TEdNotebook.Destroy;
begin
  FSheets.Free;
  FTabs.Free;
  inherited Destroy;
end;

function TEdNotebook.GetPage(Index: integer): TEdSheet;
begin
  Result := FSheets[Index] as TEdSheet;
end;

function TEdNotebook.GetActiveSheet: TEdSheet;
begin
  if (PageIndex<0)or(FSheets.Count=0) then
    Result := nil
  else
    Result := FSheets[PageIndex] as TEdSheet;
end;

function TEdNotebook.GetPageCount: integer;
begin
  Result := FSheets.Count;
end;

function TEdNotebook.GetPageIndex: integer;
begin
  Result := FTabs.FPageIndex;
end;

function TEdNotebook.GetTabHeight: integer;
begin
  Result := FTabHeight;
end;

function TEdNotebook.GetTabPosition: TTabPosition;
begin
  Result := FTabPosition;
end;

procedure TEdNotebook.SetActiveSheet(AValue: TEdSheet);
var
  Index: integer;
begin
  Index := FSheets.IndexOf(AValue);
  if Index>=0 then
    PageIndex:=Index;
end;

procedure TEdNotebook.DrawHelper(Index: Integer; IsActive: Boolean; ACanvas: TCanvas; var R: TRect; var DefaultDraw: boolean);
begin
  if Assigned(FOnDrawTab) then FOnDrawTab(self,Index,IsActive,ACanvas,R,DefaultDraw);
end;

procedure TEdNotebook.ActivateCurrent;
var
  sheet: TEdSheet;
begin
  if PageIndex<0 then exit;
  sheet := FSheets[PageIndex] as TEdSheet;
  sheet.Visible := True;
  Invalidate;
  if (sheet.FocusedControl<>nil) then
     sheet.FocusedControl.SetFocus;
end;

procedure TEdNotebook.SetPageIndex(AValue: integer);
begin
  if AValue <> PageIndex then
  begin
    (FSheets[PageIndex] as TEdSheet).Visible := False;
    FTabs.FPageIndex := AValue;
  end;
  ActivateCurrent;
end;

procedure TEdNotebook.SetTabHeight(AValue: integer);
begin
  if AValue = FTabHeight then
    exit;
  FTabHeight := AValue;
  Invalidate;
end;

procedure TEdNotebook.SetTabPosition(AValue: TTabPosition);
begin
  if AValue = FTabPosition then
    exit;
  FTabPosition := AValue;
  if FTabPosition=tpBottom then
    FTabs.Align := alBottom
  else
    FTabs.Align := alTop;
  Invalidate;
end;

procedure TEdNotebook.SetTextColorActive(AValue: TColor);
begin
  if FTextColorActive = AValue then
    Exit;
  FTextColorActive := AValue;
  Invalidate;
end;

procedure TEdNotebook.SetTextColorInactive(AValue: TColor);
begin
  if FTextColorInactive = AValue then
    Exit;
  FTextColorInactive := AValue;
  Invalidate;
end;


procedure TEdNotebook.UpdateSheet(sheet: TEdSheet);
var
  Index: integer;
begin
  Index := FSheets.IndexOf(sheet);
  if Index < 0 then
    raise Exception.Create('Not found sheet');
  FTabs.UpdateSheet(Index, sheet.Caption);
end;

function TEdNotebook.AddTabSheet(ACaption: string=''): TEdSheet;
begin
  Result := TEdSheet.Create(Self);
  Result.Height := 100;
  Result.Align := alClient;
  Result.Color := clCream;
  if (FSheets.Count > 0) then
    Result.Visible := False;
  FSheets.Add(Result);
  InsertControl(Result);
  FTabs.FTabWidths.Add(nil);
  if (FSheets.Count=1) then
    FTabs.FPageIndex:=0;
  if ACaption<>'' then
    Result.Caption := ACaption;
  UpdateSheet(Result);
end;

procedure TEdNotebook.SwapTabs(m, n: integer);
begin
  if (m = n) or (m < 0) or (m >= FSheets.Count) or (n < 0) or (m >= FSheets.Count) then
    exit;
  FSheets.Exchange(m,n);
  if PageIndex=m then
     FTabs.FPageIndex:=n
  else if PageIndex=n then
     FTabs.FPageIndex:=m;
  FTabs.SwapTabs(m, n);
  Invalidate;
end;

function TEdNotebook.GetSheetIndex(sheet: TEdSheet): Integer;
begin
  Result := FSheets.IndexOf(sheet);
end;

{ Not ask, assume that is saved, not activate other tabs}
procedure TEdNotebook.FastCloseTab(Index: integer);
begin
  RemoveControl(FSheets[Index] as TControl);
  FSheets.Delete(Index);
  FTabs.Delete(Index);
end;

procedure TEdNotebook.CloseTab(Index: integer);
begin
  if Assigned(FOnClose) then FOnClose(self, Index);
  FastCloseTab(Index);
  FTabs.Invalidate;
  ActivateCurrent;
end;

procedure TEdNotebook.TryCloseTab(Index: integer; var CanClose: TCloseEnum);
var
  Consider: TConsiderEnum;
begin
  Consider:=coCanSave;
  if Assigned(FOnBeforeCloseQuery) then FOnBeforeCloseQuery(self, Index, Consider);
  if Consider=coCanClose then
     CloseTab(Index)
  else
  begin
    PageIndex := Index;
    if Assigned(FOnCloseQuery) then FOnCloseQuery(self, Index, CanClose);
    if CanClose in [clClose, clCloseAllSave, clCloseAllDiscard] then
        CloseTab(Index)
  end
end;

procedure TEdNotebook.TryCloseSheet(sheet: TEdSheet; var CanClose: TCloseEnum);
var
  Index: integer;
begin
  Index := GetSheetIndex(sheet);
  if (Index>=0) then
    TryCloseTab(Index, CanClose)
  else
    CanClose:=clError;
end;

function TEdNotebook.TryCloseAll: TCloseEnum;
var
  i: integer;
  CanClose: TCloseEnum;
begin
  CanClose:=clClose;
  for i:=FSheets.Count-1 downto 0 do
  begin
    TryCloseTab(i, CanClose);
    if CanClose=clCancel then
    begin
      Result:=clCancel;
      exit;
    end;
  end;
  if FSheets.Count>0 then Result := clNo
  else Result := clClose;
end;

procedure TEdNotebook.WMTryCloseSheet(var Msg: TLMNotify);
var
  sheet: TEdSheet;
  CanClose: TCloseEnum;
begin
  sheet := TEdSheet(MSg.NMHdr);
  CanClose:=clClose;
  TryCloseSheet(sheet, CanClose);
end;

procedure TEdNotebook.WMTryCloseTab(var Msg: TLMNotify);
var
  Index: Integer;
  CanClose: TCloseEnum;
begin
  Index := Integer(MSg.NMHdr);
  CanClose:=clClose;
  TryCloseTab(Index, CanClose)
end;

procedure TEdNotebook.ActivePrev();
begin
  if PageCount=0 then exit;
  if PageIndex>0 then
    PageIndex := PageIndex-1
  else
    PageIndex := PageCount-1;
end;

procedure TEdNotebook.ActiveNext();
begin
  if PageCount=0 then exit;
  if PageIndex<PageCount-1 then
    PageIndex := PageIndex+1
  else
    PageIndex := 0;
end;

end.
