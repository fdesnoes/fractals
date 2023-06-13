--------------------------------------------------------------------------------
--|   Mandelbrot - The Classic Mandelbrot ensemble in Ada
--| 
--|
--|   Copyright (C) 2020 Frederic Desnoes (frederic.desnoes@wanadoo.fr)
--|
--|
--| Filename         : $Source: /fractals.ads$
--| Author           : Frederic Desnoes
--| Created On       : 2023/05/30
--| Last Modified By : $Author: Frederic Desnoes$
--| Last Modified On : $Date: 2023/05/30$
--| Status           : $State: Expe $
--|
--------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Numerics.Elementary_Functions;
with Ada.Integer_Text_Io;    use Ada.Integer_Text_Io;
with Ada.Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Sequential_IO;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces; use interfaces;
with Delay_Aux_Pkg;
with draw_mandelbrot;
with draw_julia;
with draw_buddhabrot;
with mandelbrot_types; use mandelbrot_types;

with Gnoga.Types;
with Gnoga.Types.Colors; use Gnoga.Types.Colors;
with Gnoga.Application.Singleton;
with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Element.Canvas.Context_2D; use Gnoga.Gui.Element.Canvas.Context_2D;
with Gnoga.Gui.Element.Multimedia;
with Gnoga.Gui.Element.form;



procedure fractals;

