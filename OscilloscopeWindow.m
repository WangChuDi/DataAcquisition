classdef OscilloscopeWindow < handle
    % OSCILLOSCOPEWINDOW encapsulates an axes, its figure_cache, and the
    % paired zoom/scroll buttons used throughout DataAcquisition. Creating
    % one object keeps the UI wiring in a single place so it can be reused
    % for the main view and split-view layouts.

    properties
        Parent      % uipanel or figure that owns the controls
        Axes        % Axes handle for plotting
        Cache       % figure_cache instance connected to the axes
        YButtons    % Handles for Y-axis controls
        XButtons    % Handles for X-axis controls
    end

    methods
        function obj = OscilloscopeWindow(parent, alpha, xMax, axesPosition, varargin)
            % axesPosition: normalized or pixel position for the axes
            % Optional name/value: 'YLabel'
            p = inputParser;
            addParameter(p, 'YLabel', 'Current (pA)');
            parse(p, varargin{:});

            obj.Parent = parent;
            obj.Axes = axes('Parent', parent, 'Position', axesPosition, ...
                'GridLineStyle', '-', 'XColor', 0.15*[1 1 1], 'YColor', 0.15*[1 1 1]);
            set(obj.Axes,'NextPlot','add','XLimMode','manual');
            set(obj.Axes,'XGrid','on','YGrid','on','Tag','Axes','Box','on');
            obj.Axes.XLim = [0 xMax];
            obj.Axes.YLim = [-1 1];
            obj.Axes.YLabel.String = p.Results.YLabel;
            obj.Axes.YLabel.Color = 'k';
            obj.Axes.XLabel.String = 'Time (s)';
            obj.Axes.XLabel.Color = 'k';

            obj.Cache = figure_cache(obj.Axes, alpha, xMax);
            [obj.YButtons, obj.XButtons] = obj.createAxisButtons();
        end

        function delete(obj)
            if ~isempty(obj.YButtons)
                delete(obj.YButtons(isgraphics(obj.YButtons)));
            end
            if ~isempty(obj.XButtons)
                delete(obj.XButtons(isgraphics(obj.XButtons)));
            end
            if isgraphics(obj.Axes)
                delete(obj.Axes);
            end
        end
    end

    methods (Access = private)
        function [ybuts, xbuts] = createAxisButtons(obj)
            zoomOutLabel = '-';
            scrollDownLabel = char(8595); % down arrow
            resetLabel = 'R';
            scrollUpLabel = char(8593);   % up arrow
            zoomInLabel = '+';

            ybuts = [];
            ybuts(1) = uicontrol('Parent', obj.Parent, 'String', zoomOutLabel,...
                'callback', @(~,~) obj.Cache.zoom_y('out'));
            ybuts(2) = uicontrol('Parent', obj.Parent, 'String', scrollDownLabel,...
                'callback', @(~,~) obj.Cache.scroll_y('down'));
            ybuts(3) = uicontrol('Parent', obj.Parent, 'String', resetLabel,...
                'callback', @(~,~) obj.Cache.reset_fig);
            ybuts(4) = uicontrol('Parent', obj.Parent, 'String', 'M',...
                'callback', @(~,~) obj.Cache.auto_mid);
            ybuts(5) = uicontrol('Parent', obj.Parent, 'String', scrollUpLabel,...
                'callback', @(~,~) obj.Cache.scroll_y('up'));
            ybuts(6) = uicontrol('Parent', obj.Parent, 'String', zoomInLabel,...
                'callback', @(~,~) obj.Cache.zoom_y('in'));

            xbuts = [];
            xbuts(1) = uicontrol('Parent', obj.Parent, 'String', zoomOutLabel,...
                'callback', @(~,~) obj.Cache.zoom_x('out'));
            xbuts(2) = uicontrol('Parent', obj.Parent, 'String', zoomInLabel,...
                'callback', @(~,~) obj.Cache.zoom_x('in'));
        end
    end
end
