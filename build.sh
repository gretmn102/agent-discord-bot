#!/bin/bash

dotnet tool restore
dotnet clean
dotnet paket install
dotnet fake build $@
