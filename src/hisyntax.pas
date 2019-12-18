unit hisyntax;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, intfs, SynEditHighlighterFoldBase;

type

  { THiSyntax }

  THiSyntax = class(TInterfacedObject, IHiSyntax)
  private
    fHighlighters: TStringList;
  protected
    procedure LoadStandard;
    procedure LoadFacil;
  public
    constructor Create();
    destructor Destroy;
    function GetHighlighterByLanguageName(ALang: string): TSynCustomFoldHighlighter;
    function GetHighlighterByFileName(APath: string): TSynCustomFoldHighlighter;
  end;


implementation
uses
  Graphics, SynHighlighterPas,SynHighlighterXML,SynEditStrConst, SynFacilHighlighter,
  fgl,dialogs,RegExpr;

{ THiSyntax }

procedure THiSyntax.LoadStandard;
var
  SynHL: TSynCustomFoldHighlighter;
begin
  SynHL:=TSynPasSyn.Create(nil);
  SynHL.CommentAttribute.Foreground := clTeal;
  SynHL.IdentifierAttribute.Foreground := clNavy;
  SynHL.KeywordAttribute.Style := [fsBold];
  SynHL.StringAttribute.Foreground := clFuchsia;
  SynHL.WhitespaceAttribute.Background := clCream;

  fHighlighters.AddObject(SynHL.LanguageName, SynHL);
  SynHL:=TSynXmlSyn.Create(nil);
  fHighlighters.AddObject(SynHL.LanguageName, SynHL);
end;

procedure THiSyntax.LoadFacil;
type TFMap = specialize TFPGMap<string, string>;
var
  Folder,line: string;
  result: LongInt;
  sr: TSearchRec;
  hlt : TSynFacilSyn;
  dataList: TStringList;
  map: TFMap;
  i,j: integer;
begin
  Folder:='facildata';
  map := TFMap.Create;
  dataList := TStringList.Create;
  dataList.LoadFromFile(folder+'/filters.dat');
  for i:=0 to dataList.Count-1 do
  begin
    line:=dataList[i];
    j := Pos('|', line);
    if j <= 0 then continue;
    map.Add(Copy(line,1,j-1), Copy(line,j+1));
  end;
  dataList.Free;
  result :=  FindFirst(folder+'/*.xml', faAnyFile, sr);
  while result = 0 do
  begin
    hlt := TSynFacilSyn.Create(nil);
    hlt.LoadFromFile(Folder+'/'+sr.Name);
    hlt.DefaultFilter:=map[sr.Name];
    fHighlighters.AddObject(ExtractFileName(sr.Name), hlt);
    result := FindNext(sr);
  end;
  FindClose(sr);
  map.Free;
end;

constructor THiSyntax.Create();
begin
  fHighlighters:=TStringList.Create;
  LoadStandard;
  LoadFacil;
end;

destructor THiSyntax.Destroy;
var
  i: integer;
begin
  for i:=0 to fHighlighters.Count-1 do
    fHighlighters.Objects[i].Free;
  fHighlighters.Free;
end;

function THiSyntax.GetHighlighterByLanguageName(ALang: string): TSynCustomFoldHighlighter;
var
  Index: integer;
begin
  Index := fHighlighters.IndexOf(ALang);
  if Index>=0 then
    Result := fHighlighters.Objects[Index] as TSynCustomFoldHighlighter
  else
    Result := nil;
end;

function THiSyntax.GetHighlighterByFileName(APath: string): TSynCustomFoldHighlighter;
var
  name,ext,Filter,mask: string;
  StrArray: TStringArray;
  synHL: TSynCustomFoldHighlighter;
  i,n,j,ExtLen: integer;
  re: TRegExpr;
begin
  name:=ExtractFileName(APath);
  Result := nil;
  ext := ExtractFileExt(APath);
  ExtLen := Length(ext);
  for i:=0 to fHighlighters.Count-1 do
  begin
    synHL := fHighlighters.Objects[i] as TSynCustomFoldHighlighter;
    Filter := synHL.DefaultFilter;
    n := Pos('|', Filter);
    if n > 0 then
    begin
      Delete(Filter, 1, n);
      StrArray:=Filter.Split(';');
      for j:=0 to Length(StrArray)-1 do
      begin
        mask:=StringReplace(StrArray[j],'.','\.',[rfReplaceAll]);
        mask:=StringReplace(mask,'*','.*',[rfReplaceAll]);
        re := TRegExpr.Create(mask);
        if re.Exec(name) then
        begin
           Result:=synHL;
           re.Free;
           exit;
        end else
           re.Free;
      end;
    end;
  end;
end;

end.

