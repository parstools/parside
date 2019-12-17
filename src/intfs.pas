unit intfs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  IDocument = interface ['{33F35F97-56E0-44FD-A0AA-5EA0CAD1C6B5}']
    function GetPath: string;
    function GetTitle: string;
    procedure OpenFile(AFileName: string);
    function ComparePathWith(AOtherPath: string): integer;
    procedure Activate;
    function TryClose(): Boolean;
  end;

  IUntitledManager = interface ['{476DA6F2-AAED-4923-BBBA-6726E6F8914F}']
    function GetNewNumber: integer;
    procedure ReleaseNumber(ANumber: integer);
  end;

  IDocumentFactory = interface ['{9FC64814-516F-4344-9961-FC0D45A7F3CA}']
    function CreateNew(AFileName: string): IDocument;
    function GetActive: IDocument;
    function GetUntitledManager: IUntitledManager;
  end;

implementation

end.

