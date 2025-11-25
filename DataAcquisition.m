classdef DataAcquisition < handle
    % DATAACQUISITION is a Matlab program for electronic signal acquistion from
    % a National Instruments DAQ USB-6003.
    %
    % A graphical user interface can be launched from the command line in
    % Matlab by typing
    % >> d = DataAcquisition('Channels',[0,1], 'Alphas',[2000,1000], 'OutputAlpha',1,'SampleFrequency',50000);
    % at the command prompt.
    %
    % This software was written with Patch Clamp and electrophysiological
    % measurements in mind, although it could also be used as a simple
    % oscilloscope and data recording tool whenever measurements are made using
    % a DAQ from National Instruments.
    %
    % DataAcquisition has the following modes of operation, which can be
    % accessed via the Mode menu.
    %
    % "Normal Acquisition":
    %   An oscilloscope mode, enabling live data viewing as well as "gap-free"
    %   recording.
    % "Noise Plot":
    %   A live noise plot is displayed on the screen and is periodically
    %   refreshed, allowing the user to identify noise sources, make changes in
    %   an experiment, and see their effects in real time.
    % "IV Curve":
    %   Programmable, periodic voltage stimulation allows the user to make a
    %   recording of current versus voltage.  This feature displays the results
    %   on screen in a second panel.
    % "Membrane Seal Test":
    %   A mode used for examining capacitance.  Reapeatedly
    %   applies a voltage step function and displays the resulting current
    %   response.  This mode is useful in patch clamp experiments, where the
    %   user can adjust capacitance compensation to offset the current spikes
    %   which accompany voltage steps that are applied across a membrane.
    %
    % The configuration of the DAQ, including scale factors specific to the
    % current amplifier being used, can be set by navigating to
    % File > Configure DAQ.
    %
    % Use of the functionality specific to electrophysiology measurements (IV
    % curve and membrane seal test) require the analog output terminal 'ao0' of
    % the DAQ to be connected to an external command input on the current
    % amplifier.
    %
    % Data is saved in a .dbf file format in the folder specified by the user
    % (can be set by navigating to File > Choose Save Directory).
    % The .dbf file format, for "Dataacquisition Binary Format," is a binary
    % file which contains a header followed by the signal data, and can be
    % opened using Tamas Szalay's PoreView software, with the appropriate
    % additions for .dbf handling.
    %
    % Data in .dbf files can also be converted to other filetypes for use in
    % other data analysis pipelines.  Data can be converted via
    % File > Convert Data File
    % using the dialog that pops up.
    %
    % Stephen Fleming 2016.08.13
    % latest updates 2018.04.100

    properties
        fig         % Handle for the entire DataAcquisition figure
        DAQ         % Matlab DAQ object handle
        file        % Information about the next data file to be saved
        mode        % Which program is running: normal, noise, iv, sealtest
        alpha       % Conversion factors from input to real signal
        outputAlpha % Conversion factor for output
        channels    % Channels for data acquisition
        sampling    % Sampling frequency for input channels
        allChannels % All 8 channels (0-7) for display
        channelEnabled  % Which channels are enabled for acquisition
        differentialPairs  % Differential channel pairs {[0,4], [1,5], [2,6], [3,7]}
        saveChannelFlags % Which channels should be saved to file (handles differential mode)
    end

    properties (Hidden=true)
        panel       % Handle for main display panel
        axes        % Handle for the main axes
        fig_cache   % Handle for cache object that handles live data
        DEFS        % UI definitions (widths etc.)
        tabs        % Main window tabs handle struct
        hcmenu      % Handle to context menu
        tbuts       % Handles for the main control buttons
        ybuts       % Handles for the y axis buttons
        xbuts       % Handles for the x axis buttons
        splitButton % Handle for the split view toggle button
        chbuts      % Handles for the channel toggle buttons
        channelVisible  % Array to track which channels are visible
        outputFreq  % Frequency of analog output signal
        audio       % Matlab audio player object
        data_compression_scaling    % compression to int16

        % Split view handling
        splitMode = false           % Whether split view is enabled
        subplotAxes = gobjects(1,0) % Handles for split axes
        subplotCaches = {}          % figure_cache objects for split axes
        splitYButtons = {}          % Y-axis button handles for split axes
        splitXButtons = {}          % X-axis button handles for split axes
    end

    methods (Hidden=true)

        function obj = DataAcquisition(varargin)
            % Constructor

            % Handle inputs
            inputs = obj.parseInputs(varargin);
            obj.channels = inputs.Channels;
            inputAlphas = inputs.Alphas;
            obj.sampling = inputs.SampleFrequency;
            obj.outputAlpha = inputs.OutputAlpha;
            
            % Initialize all 8 channels and their states
            obj.allChannels = 0:7;
            obj.channelEnabled = false(1, 8);
            obj.channelEnabled(obj.channels + 1) = true; % +1 for 1-based indexing
            
            % Always use all 8 channels for DAQ, configure display based on enabled channels
            obj.channels = 0:7;  % Always acquire all 8 channels
            
            % Expand alpha to 8 elements (use first alpha value for unspecified channels)
            obj.alpha = ones(1, 8) * inputAlphas(1);
            for i = 1:numel(inputAlphas)
                obj.alpha(inputs.Channels(i) + 1) = inputAlphas(i);  % Map to correct channel
            end
            
            % Initialize channel visibility (only enabled channels visible by default)
            obj.channelVisible = obj.channelEnabled;
            
            % Initialize differential pairs (each pair is [low, high])
            obj.differentialPairs = {[0,4], [1,5], [2,6], [3,7]};
            
            % Initialize save channel flags (by default, save all enabled channels)
            obj.saveChannelFlags = obj.channelEnabled;

            % Figure out scale factor for data compression
            % (i.e. lossless representation as a 16-bit integer)
            obj.data_compression_scaling = (2^15-1)./obj.alpha/10; % range of 10V

            % Set constants
            obj.outputFreq = 5000; %

            % Initialize DAQ
            function startDAQ(~,~)
                try
                    obj.DAQ.s = daq.createSession('ni');
                    % First add channels, then set the rate
                    addAnalogInputChannel(obj.DAQ.s, 'Dev1', obj.channels, 'Voltage');
                    % Set rate after channels are configured (based on 8 channels = 12.5kHz each)
                    obj.DAQ.s.Rate = 12500;  % Safe rate for 8 channels

                    obj.DAQ.ao0 = daq.createSession('ni');
                    addAnalogOutputChannel(obj.DAQ.ao0, 'Dev1', 'ao0', 'Voltage');%'ao0'
                    obj.DAQ.ao0.Rate = obj.outputFreq;
                    obj.DAQ.ao1 = daq.createSession('ni');
                    addAnalogOutputChannel(obj.DAQ.ao1, 'Dev1', 'ao1', 'Voltage');%'ao1'
                    obj.DAQ.ao1.Rate = obj.outputFreq;

                    obj.DAQ.s.IsContinuous = true;

                    queueOutputData(obj.DAQ.ao1,[0*ones(1,5)]'*obj.outputAlpha); %liyi 2021-03-12
                    obj.DAQ.ao1.startBackground; %liyi 2021-03-12
                    obj.DAQ.ao1.stop;
                    obj.DAQ.ao1.IsContinuous = false;

                    queueOutputData(obj.DAQ.ao0,[zeros(1,5)]'*obj.outputAlpha); %liyi 2021-03-12
                    obj.DAQ.ao0.startBackground; %liyi 2021-03-12
                    obj.DAQ.ao0.stop;
                    obj.DAQ.ao0.IsContinuous = false;

                    display('DAQ communication established...')
                catch ex
                    display('Problem initializing DAQ!')
                end
                try
                    % pre-run to warm up and prevent lag later
                    obj.DAQ.listeners.plot = addlistener(obj.DAQ.s, 'DataAvailable', ...
                        @(~,~) 0);
                    obj.DAQ.s.startBackground;
                    obj.DAQ.s.stop;
                    display('DAQ successfully initialized.')
                catch ex

                end
            end
            startDAQ;

            % Initialize temporary file and file data
            function setFileLocation(~,~)
                c = clock;
                % get the folder location from the user
                obj.file.folder = [uigetdir(['C:\Data\PatchClamp\' num2str(c(1)) ...
                    sprintf('%02d',c(2)) sprintf('%02d',c(3))], ...
                    'Select save location') '\'];
                % if unable to get input from user
                if any([isempty(obj.file.folder), obj.file.folder == 0])
                    obj.file.folder = ['C:\Data\PatchClamp\' num2str(c(1)) sprintf('%02d',c(2)) sprintf('%02d',c(3)) '\'];
                end
                obj.file.prefix = [num2str(c(1)) '_' sprintf('%02d',c(2)) '_' sprintf('%02d',c(3))];
                obj.file.suffix = '.dbf';
                obj.file.num = 0;
                obj.file.name = [obj.file.folder obj.file.prefix '_' sprintf('%04d',obj.file.num) obj.file.suffix];
                obj.file.fid = [];
            end
            setFileLocation;

            % Create space for listeners
            obj.DAQ.listeners = [];

            % Enable the user to change DAQ configuation settings
            function configure(~,~)
                % open up a dialog box where users can change the DAQ
                % configuration

                prompt = {'Channels (array of integers):', ...
                    'Scaling of inputs (array: measured*scaling = pA or mV):', ...
                    'Scaling of output (number: output(mV)*scaling = Volts in DAQ''s output range)', ...
                    'Sampling frequency (number in Hz):'};
                dlg_title = 'DAQ configuration';
                defaultans = {['[' num2str(obj.channels) ']'], ...
                    ['[' num2str(obj.alpha) ']'], ...
                    num2str(obj.outputAlpha), ...
                    num2str(obj.sampling)};
                answer = inputdlg(prompt,dlg_title,1,defaultans);
                if ~isempty(answer) % user did not press 'cancel'
                    % construct the argument list of name value pairs
                    emptyinputs = cellfun(@(x) isempty(x), answer);
                    names = {'Channels','Alphas','OutputAlpha','SampleFrequency'};
                    values = cellfun(@(x) str2num(x), answer, 'uniformoutput', false);
                    listargs = reshape([names(~emptyinputs); values(~emptyinputs)'],1,[]);

                    % re-configure the DAQ
                    inputs = obj.parseInputs(listargs);
                    obj.channels = inputs.Channels;
                    obj.alpha = inputs.Alphas;
                    obj.sampling = inputs.SampleFrequency;
                    obj.outputAlpha = inputs.OutputAlpha;

                    % update parameters in DAQ config and in figure cache
                    startDAQ;
                    obj.mode = 'normal';
                    obj.normalMode;
                end

            end

            % Enable the user to convert DBF file to another filetype
            function convertDataFile(~,~)
                % convert file from the DBF filetype to another filetype,
                % either HDF or CSV, which will be larger files, but can be
                % used in any data analysis pipeline.

                % open a dialog to prompt user to select a file for
                % conversion
                [dataFile, dataPath] = uigetfile([obj.file.folder '/*.dbf'], ...
                    'Select DBF file to convert');

                % if unable to get input file from user
                if any([isempty(dataFile), dataFile == 0])
                    display('Conversion aborted.')
                    return;
                end

                % pop up a dialog to let user choose desired file type
                filetype = questdlg('What is the desired output file type?', ...
                    'File converter', 'hdf', 'csv', 'Cancel', 'Cancel');

                % if user chose to cancel
                if strcmp(filetype,'Cancel')
                    display('Conversion aborted.')
                    return;
                end

                % set up new file, same name .csv or .hdf
                newFileName = [dataPath, dataFile(1:end-3), filetype];
                import matlab.io.hdf4.* % scope this import for the whole

                try

                    % open data file
                    oldFileName = [dataPath, dataFile];
                    [~, h] = dbfload(oldFileName, 'info');
                    chunk = 2e5; % 200k data points at a time

                    % if CSV, display metadata at command window
                    if strcmp(filetype,'csv')
                        display('Important file metadata will not be saved with CSV.')
                        display('File metadata:')
                        display(['Sampling interval = ' num2str(h.si) ' seconds'])
                        display('Channels recorded:')
                        display(h.chNames)
                        display(['Number of data points recorded (for each channel) = ' num2str(h.numPts)])
                        % put in initial headers in csv file
                        fid = fopen(newFileName, 'W'); % capital W
                        fmt = [repmat('%s,', 1, length(h.chNames)) '\r\n'];
                        fprintf(fid, fmt, h.chNames{:});
                        fmtstr = [repmat('%.3f,', 1, length(h.chNames)) '\r\n'];
                        fmt = repmat(fmtstr, 1, chunk);
                    else
                        % it's an hdf file, which needs to be initialized
                        %delete newFileName % this seems to be necessary
                        sdID =  sd.start(newFileName,'create');
                        % write meta-data from header into the hdf file
                        sd.setAttr(sdID,'sampling_interval',h.si);
                        sd.setAttr(sdID,'samples_per_channel',h.numPts);
                        sd.setAttr(sdID,'number_of_channels',h.numChan);
                        sd.setAttr(sdID,'convert_int16_to_double_by_dividing_by',h.data_compression_scaling);
                        for i = 0:h.numChan-1
                            sdsIDs(i+1) = sd.create(sdID,h.chNames{i+1},'int16',h.numPts);
                            % calibration to go from 16-bit int to actual value
                            sd.setCal(sdsIDs(i+1),1/h.data_compression_scaling(i+1),0,0,0,'int16'); % datatype of uncalibrated data
                            % info
                            sd.setDataStrs(sdsIDs(i+1),[h.chNames{i+1} ' = int16 value * scale_factor'],h.chNames{i+1}(end-2:end-1), ...
                                'int16',sprintf('Timepoints sampled at intervals of %g seconds', h.si));
                        end
                    end

                    % load data in chunks and save in new format
                    fprintf('Converting dbf file to %s ...  %3.0f%% ',filetype,0)
                    for i = 0:chunk:h.numPts
                        % load chunk
                        range = [i, min(h.numPts, i+chunk)];
                        % load and write chunk
                        if strcmp(filetype,'csv')
                            [d, ~] = dbfload(oldFileName, range, 'double');
                            % dlmwrite(newFileName,d,'-append','precision',5,'newline','pc'); % slow
                            if size(d,1) < chunk
                                fmt  = repmat(fmtstr, 1, size(d,1)); % last chunk is smaller
                            end
                            fprintf(fid, fmt, d');%liyi 2021-03-29
                        elseif strcmp(filetype,'hdf')% it's hdf
                            [d, ~] = dbfload(oldFileName, range, 'int16');
                            for chan = 1:size(d,2)
                                sd.writeData(sdsIDs(chan), i, d(:,chan));
                            end
                        end
                        % track completion
                        fprintf('\b\b\b\b\b%3.0f%% ',100*min([1, (i+chunk)/h.numPts]))
                    end
                    fprintf('\n')

                    % close file
                    if strcmp(filetype,'hdf')
                        arrayfun(@(x) sd.endAccess(x), sdsIDs);
                        sd.close(sdID);
                    elseif strcmp(filetype,'csv')
                        fclose(fid);
                    end
                    display(['Successfully saved new file ' newFileName])

                catch ex % something went wrong...

                    % give up files
                    if strcmp(filetype,'hdf')
                        try
                            arrayfun(@(x) sd.endAccess(x), sdsIDs);
                            sd.close(sdID);
                        catch ex
                        end
                    elseif strcmp(filetype,'csv')
                        try
                            fclose(fid);
                        catch ex
                        end
                    end
                    display('Something went wrong in the conversion... do you have write access?')

                end

            end

            % Define what happens on close
            function closeProg(~,~)
                % close figure
                delete(obj.fig)
                % delete stuff
                try
                    arrayfun(@(lh) delete(lh), obj.DAQ.listeners);
                    delete(obj.fig_cache);
                catch ex

                end
                % end DAQ sessions
                try
                    stop(obj.DAQ.s);
                    stop(obj.DAQ.ao0);
                    stop(obj.DAQ.ao1);
                    release(obj.DAQ.s);
                    release(obj.DAQ.ao0);
                    release(obj.DAQ.ao1);
                    delete(obj.DAQ);
                catch ex

                end
                % delete the DataAcquistion object
                try
                    delete(obj);
                catch ex

                end
            end

            % Create figure ===============================================

            % some UI defs
            obj.DEFS = [];
            obj.DEFS.BIGBUTTONSIZE  = 35;
            obj.DEFS.BUTTONSIZE     = 20;
            obj.DEFS.PADDING        = 2;
            obj.DEFS.LABELWIDTH     = 65;

            % start making GUI objects
            obj.fig = figure('Name','DataAcquisition','MenuBar','none',...
                'NumberTitle','off','DockControls','off','Visible','off', ...
                'DeleteFcn',@closeProg);

            % set its position
            oldunits = get(obj.fig,'Units');
            set(obj.fig,'Units','normalized');
            set(obj.fig,'Position',[0.1,0.1,0.8,0.8]);
            set(obj.fig,'Units',oldunits);

            % make the menu bar
            f = uimenu('Label','File');
            uimenu(f,'Label','Choose Save Directory','Callback',@setFileLocation);
            uimenu(f,'Label','Configure DAQ','Callback',@configure);
            uimenu(f,'Label','Convert Data File','Callback',@convertDataFile);
            uimenu(f,'Label','Quit','Callback',@closeProg);

            mm = uimenu('Label','Mode');
            uimenu(mm,'Label','Normal acquisition','Callback',@(~,~) obj.normalMode);
            uimenu(mm,'Label','IV curve','Callback',@(~,~) obj.ivMode);
            uimenu(mm,'Label','Noise plot','Callback',@(~,~) obj.noiseMode);
            uimenu(mm,'Label','Membrane seal test','Callback',@(~,~) obj.sealtestMode);
            uimenu(mm,'Label','Frequency response','Callback',@(~,~) obj.freqrespMode);

            hm = uimenu('Label','Help');
            uimenu(hm,'Label','DataAcquisition','Callback',@(~,~) doc('DataAcquisition.m'));
            uimenu(hm,'Label','About','Callback',@(~,~) msgbox({'DataAcquisition v1.0 - written by Stephen Fleming.' '' ...
                'Created for Harvard''s 2016 Freshaman Seminar course 25o.' '' ...
                'This program and its author are not affiliated with National Instruments, Matlab, or Molecular Devices.' ''},'About DataAcquisition'));

            obj.mode = 'normal';
            obj.normalMode;

            % =============================================================

        end

        function normalMode(obj)
            % stop other things that might have been running
            obj.stopAll;
            obj.mode = 'normal';

            % set default positions
            obj.normalSizing;
            obj.normalResizeFcn;

            % Reset split view controls for normal mode
            obj.splitMode = false;
            obj.clearSplitView();
            if ~isempty(obj.splitButton) && isvalid(obj.splitButton)
                set(obj.splitButton, 'Value', 0);
            end

            % Initialize figure cache with all 8 channels
            obj.fig_cache = figure_cache(obj.axes, obj.alpha, 5);
            
            % Set initial channel visibility based on enabled state
            obj.updateChannelDisplay();

            % Show figure
            obj.fig.Visible = 'on';

        end

        function normalSizing(obj)
            delete(obj.panel);
            obj.panel = uipanel('Parent',obj.fig,'Position',[0 0 1 1]);

            delete(obj.axes);
            obj.axes = axes('Parent',obj.panel,'Position',[0.05 0.05 0.9 0.9],...
                'GridLineStyle','-','XColor', 0.15*[1 1 1],'YColor', 0.15*[1 1 1]);
            set(obj.axes,'NextPlot','add','XLimMode','manual');
            set(obj.axes,'XGrid','on','YGrid','on','Tag','Axes','Box','on');
            obj.axes.XLim = [0 5];
            obj.axes.YLim = [-1 1];
            obj.axes.YLabel.String = 'Current (pA)';
            obj.axes.YLabel.Color = 'k';
            obj.axes.XLabel.String = 'Time (s)';
            obj.axes.XLabel.Color = 'k';

            % now make the buttons

            % Use plain-text labels instead of HTML for MATLAB UI compatibility
            zoomOutLabel = '-';
            scrollDownLabel = char(8595); % down arrow
            resetLabel = 'R';
            scrollUpLabel = char(8593);   % up arrow
            zoomInLabel = '+';

            % y-axis
            obj.ybuts = [];
            obj.ybuts(1) = uicontrol('Parent', obj.panel, 'String', zoomOutLabel,...
                'callback', @(~,~) obj.fig_cache.zoom_y('out'));
            obj.ybuts(2) = uicontrol('Parent', obj.panel, 'String', scrollDownLabel,...
                'callback', @(~,~) obj.fig_cache.scroll_y('down'));
            obj.ybuts(3) = uicontrol('Parent', obj.panel, 'String', resetLabel,...
                'callback', @(~,~) obj.fig_cache.reset_fig);
            obj.ybuts(4) = uicontrol('Parent', obj.panel, 'String', scrollUpLabel,...
                'callback', @(~,~) obj.fig_cache.scroll_y('up'));
            obj.ybuts(5) = uicontrol('Parent', obj.panel, 'String', zoomInLabel,...
                'callback', @(~,~) obj.fig_cache.zoom_y('in'));

            % x-axis
            obj.xbuts = [];
            obj.xbuts(1) = uicontrol('Parent', obj.panel, 'String', zoomOutLabel,...
                'callback', @(~,~) obj.fig_cache.zoom_x('out'));
            obj.xbuts(2) = uicontrol('Parent', obj.panel, 'String', zoomInLabel,...
                'callback', @(~,~) obj.fig_cache.zoom_x('in'));

            % split view toggle
            obj.splitButton = uicontrol('Parent', obj.panel, 'Style', 'togglebutton', ...
                'String', 'Split', 'FontWeight', 'bold', ...
                'callback', @(src,~) obj.toggleSplitView(src));

            % top
            obj.tbuts = [];
            obj.tbuts(1) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_NEGZAP_On.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'set_negative_zap_voltage');
            obj.tbuts(2) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_NV_Off.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'set_negative_higher_voltage');
            obj.tbuts(3) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_NV_Off.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'set_negative_high_voltage');
            obj.tbuts(4) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_NV_Off.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'set_negative_midtwo_voltage');
            obj.tbuts(5) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_NV_Off.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'set_negative_mid_voltage');
            obj.tbuts(6) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_NV_Off.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'set_negative_low_voltage');
            obj.tbuts(7) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Record.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'record');
            obj.tbuts(8) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Play.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'play');
            obj.tbuts(9) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_PV_Off.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'set_positive_low_voltage');
            obj.tbuts(10) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_PV_Off.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'set_positive_mid_voltage');
            obj.tbuts(11) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_PV_Off.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'set_positive_midtwo_voltage');
            obj.tbuts(12) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_PV_Off.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'set_positive_high_voltage');
            obj.tbuts(13) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_PV_Off.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'set_positive_higher_voltage');
            obj.tbuts(14) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_POSZAP_On.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'set_positive_zap_voltage');
            obj.tbuts(15) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Gauge.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'set_gauge_value');
            
            % Channel toggle buttons - create for all 8 channels
            obj.chbuts = [];
            
            % Get MATLAB default color order (same as used in plots)
            colorOrder = get(groot,'defaultaxescolororder');
            % Extend to 8 colors if needed
            if size(colorOrder, 1) < 8
                colorOrder = repmat(colorOrder, ceil(8/size(colorOrder,1)), 1);
            end
            
            % Define color scheme
            disabledColor = [0.7 0.7 0.7];   % Light gray for disabled
            diffColor = [0.1 0.5 0.9];       % Blue for differential
            
            for i = 1:8
                channelNum = i - 1; % Channel 0-7
                isEnabled = obj.channelEnabled(i);
                
                % Use plot color for enabled channels, gray for disabled
                if isEnabled
                    bgColor = colorOrder(i, :);
                else
                    bgColor = disabledColor;
                end
                
                obj.chbuts(i) = uicontrol('Parent', obj.panel, ...
                    'Style', 'togglebutton', 'String', ['CH' num2str(channelNum)], ...
                    'FontSize', 8, 'FontWeight', 'bold', ...
                    'ForegroundColor', [1 1 1], ...
                    'BackgroundColor', bgColor, ...
                    'callback', @(src,~) obj.toggleChannel(src, i), ...
                    'Value', isEnabled, 'tag', ['channel_' num2str(i)], ...
                    'UserData', struct('isDifferential', false, 'pairChannel', -1, ...
                                      'channelColor', colorOrder(i, :), 'disabledColor', disabledColor, 'diffColor', diffColor));
                
                % Add context menu for differential mode (only for channels 0-3)
                if channelNum < 4
                    cmenu = uicontextmenu;
                    uimenu(cmenu, 'Label', ['Toggle Differential ' num2str(channelNum) '-' num2str(channelNum+4)], ...
                        'Callback', @(~,~) obj.toggleDifferentialPair(i));
                    set(obj.chbuts(i), 'UIContextMenu', cmenu);
                end
            end
            
            % set the resize function
            set(obj.panel, 'ResizeFcn', @(~,~) obj.normalResizeFcn);
            % and call it to set default positions
            obj.normalResizeFcn;

            % Initialize figure cache with all 8 channels
            obj.fig_cache = figure_cache(obj.axes, obj.alpha, 5);

            % Show figure
            obj.fig.Visible = 'on';

        end

        function normalResizeFcn(obj)
            % Switch layout depending on split view state
            if obj.splitMode
                obj.layoutSplitAxes();
                return;
            end

            % get size of panel in pixels
            sz = obj.getPixelPos(obj.panel);
            % position the axes object
            sz(1) = sz(1) + obj.DEFS.PADDING + obj.DEFS.LABELWIDTH + obj.DEFS.BUTTONSIZE; % left
            sz(3) = sz(3) - sz(1); % width
            sz(2) = sz(2) + obj.DEFS.PADDING + obj.DEFS.LABELWIDTH + obj.DEFS.BUTTONSIZE; % bottom
            sz(4) = sz(4) - sz(2) - obj.DEFS.BIGBUTTONSIZE - 3*obj.DEFS.PADDING; % height
            set(obj.axes,'Position',max(1,sz),'Units','Pixels');
            % get size of axes in pixels
            sz = obj.getPixelPos(obj.axes);
            % figure out where the y middle is
            midle = sz(4)/2 + sz(2);
            % position the buttons
            for i=1:numel(obj.ybuts)
                set(obj.ybuts(i),'Position', ...
                    max(1,[obj.DEFS.PADDING, ...
                    midle+(i-numel(obj.ybuts)/2-1)*obj.DEFS.BUTTONSIZE, ...
                    obj.DEFS.BUTTONSIZE, ...
                    obj.DEFS.BUTTONSIZE]));
            end
            % figure out where the x middle is
            midle = sz(3)/2 + sz(1);
            % position the buttons
            for i=1:numel(obj.xbuts)
                set(obj.xbuts(i),'Position', ...
                    max(1,[midle+(i-numel(obj.xbuts)/2-1)*obj.DEFS.BUTTONSIZE, ...
                    obj.DEFS.PADDING, ...
                    obj.DEFS.BUTTONSIZE, ...
                    obj.DEFS.BUTTONSIZE]));
            end
            % position the split toggle button to the right of x-buttons
            if ~isempty(obj.splitButton) && isvalid(obj.splitButton)
                set(obj.splitButton, 'Position', ...
                    max(1,[midle + (numel(obj.xbuts)/2+0.5)*obj.DEFS.BUTTONSIZE, ...
                    obj.DEFS.PADDING, obj.DEFS.BIGBUTTONSIZE, obj.DEFS.BUTTONSIZE]));
            end
            % position the channel toggle buttons (above x-axis buttons)
            if ~isempty(obj.chbuts)
                for i=1:numel(obj.chbuts)
                    set(obj.chbuts(i),'Position', ...
                        max(1,[midle - (numel(obj.chbuts)/2 - i + 0.5)*obj.DEFS.BIGBUTTONSIZE, ...
                        obj.DEFS.PADDING + obj.DEFS.BUTTONSIZE + obj.DEFS.PADDING, ...
                        obj.DEFS.BIGBUTTONSIZE, ...
                        obj.DEFS.BUTTONSIZE]));
                end
            end
            for i=1:numel(obj.tbuts)
                set(obj.tbuts(i),'Position', ...
                    max(1,[midle+(i-numel(obj.tbuts)/2-1)*obj.DEFS.BIGBUTTONSIZE, ...
                    sz(2) + sz(4) + obj.DEFS.PADDING, ...
                    obj.DEFS.BIGBUTTONSIZE, ...
                    obj.DEFS.BIGBUTTONSIZE]));
            end
        end

        function toggleSplitView(obj, src)
            obj.splitMode = logical(get(src, 'Value'));

            if obj.splitMode
                obj.createSplitAxes();
            else
                obj.clearSplitView();
                if isgraphics(obj.axes)
                    obj.axes.Visible = 'on';
                end
                if ~isempty(obj.fig_cache)
                    obj.fig_cache.clear_fig();
                    obj.fig_cache.draw_fig_now();
                end
            end

            obj.normalResizeFcn();
        end

        function createSplitAxes(obj)
            obj.clearSplitView();

            visibleChannels = find(obj.channelVisible);
            if isempty(visibleChannels)
                obj.splitMode = false;
                if ~isempty(obj.splitButton) && isvalid(obj.splitButton)
                    set(obj.splitButton, 'Value', 0);
                end
                return;
            end

            nAxes = numel(visibleChannels);
            obj.subplotAxes = gobjects(1, nAxes);
            obj.subplotCaches = cell(1, nAxes);
            obj.splitYButtons = cell(1, nAxes);
            obj.splitXButtons = cell(1, nAxes);

            xMaxForSplit = 5;
            if ~isempty(obj.fig_cache)
                xMaxForSplit = obj.fig_cache.xmax;
            end

            for idx = 1:nAxes
                ax = axes('Parent', obj.panel, 'GridLineStyle','-', ...
                    'XColor', 0.15*[1 1 1],'YColor', 0.15*[1 1 1]);
                set(ax,'NextPlot','add','XLimMode','manual');
                set(ax,'XGrid','on','YGrid','on','Tag','Axes','Box','on');
                ax.XLabel.String = 'Time (s)';
                ax.XLabel.Color = 'k';
                ax.YLabel.String = sprintf('CH%d', visibleChannels(idx)-1);
                ax.YLabel.Color = 'k';
                ax.XLim = obj.axes.XLim;
                ax.YLim = obj.axes.YLim;

                obj.subplotAxes(idx) = ax;

                chAlpha = obj.alpha(visibleChannels(idx));
                chCache = figure_cache(ax, chAlpha, xMaxForSplit);
                chCache.channelVisible = true(1,1);
                obj.subplotCaches{idx} = chCache;

                [obj.splitYButtons{idx}, obj.splitXButtons{idx}] = obj.createAxisButtons(chCache);
            end

            if isgraphics(obj.axes)
                obj.axes.Visible = 'off';
            end

            obj.layoutSplitAxes();
        end

        function [ybuts, xbuts] = createAxisButtons(obj, cache)
            % Use plain-text labels instead of HTML for MATLAB UI compatibility
            zoomOutLabel = '-';
            scrollDownLabel = char(8595); % down arrow
            resetLabel = 'R';
            scrollUpLabel = char(8593);   % up arrow
            zoomInLabel = '+';

            ybuts = [];
            ybuts(1) = uicontrol('Parent', obj.panel, 'String', zoomOutLabel,...
                'callback', @(~,~) cache.zoom_y('out'));
            ybuts(2) = uicontrol('Parent', obj.panel, 'String', scrollDownLabel,...
                'callback', @(~,~) cache.scroll_y('down'));
            ybuts(3) = uicontrol('Parent', obj.panel, 'String', resetLabel,...
                'callback', @(~,~) cache.reset_fig);
            ybuts(4) = uicontrol('Parent', obj.panel, 'String', scrollUpLabel,...
                'callback', @(~,~) cache.scroll_y('up'));
            ybuts(5) = uicontrol('Parent', obj.panel, 'String', zoomInLabel,...
                'callback', @(~,~) cache.zoom_y('in'));

            xbuts = [];
            xbuts(1) = uicontrol('Parent', obj.panel, 'String', zoomOutLabel,...
                'callback', @(~,~) cache.zoom_x('out'));
            xbuts(2) = uicontrol('Parent', obj.panel, 'String', zoomInLabel,...
                'callback', @(~,~) cache.zoom_x('in'));
        end

        function layoutSplitAxes(obj)
            if isempty(obj.subplotAxes) || ~all(isgraphics(obj.subplotAxes))
                return;
            end

            % Base layout numbers similar to normal view
            panelPos = obj.getPixelPos(obj.panel);
            left = obj.DEFS.PADDING + obj.DEFS.LABELWIDTH + obj.DEFS.BUTTONSIZE;
            width = panelPos(3) - left;
            bottomBase = obj.DEFS.PADDING + obj.DEFS.LABELWIDTH + obj.DEFS.BUTTONSIZE;
            heightAvail = panelPos(4) - bottomBase - obj.DEFS.BIGBUTTONSIZE - 3*obj.DEFS.PADDING;

            nAxes = numel(obj.subplotAxes);
            segmentHeight = heightAvail / nAxes;

            for idx = 1:nAxes
                if ~isgraphics(obj.subplotAxes(idx))
                    continue;
                end
                segmentBottom = bottomBase + (nAxes - idx) * segmentHeight;
                axHeight = max(1, segmentHeight - obj.DEFS.BUTTONSIZE - obj.DEFS.PADDING);
                axPos = [left, segmentBottom + obj.DEFS.BUTTONSIZE, ...
                    width - obj.DEFS.BUTTONSIZE, axHeight];

                set(obj.subplotAxes(idx), 'Position', max(1, axPos), 'Units', 'Pixels');

                % y buttons aligned to axis center
                midY = axPos(2) + axPos(4)/2;
                ybuts = obj.splitYButtons{idx};
                for j = 1:numel(ybuts)
                    if isgraphics(ybuts(j))
                        set(ybuts(j), 'Position', max(1, [obj.DEFS.PADDING, ...
                            midY + (j-numel(ybuts)/2-1)*obj.DEFS.BUTTONSIZE, ...
                            obj.DEFS.BUTTONSIZE, obj.DEFS.BUTTONSIZE]));
                    end
                end

                % x buttons below each axis
                midX = axPos(1) + axPos(3)/2;
                xbuts = obj.splitXButtons{idx};
                for j = 1:numel(xbuts)
                    if isgraphics(xbuts(j))
                        set(xbuts(j), 'Position', max(1, [midX + (j-numel(xbuts)/2-1)*obj.DEFS.BUTTONSIZE, ...
                            segmentBottom, obj.DEFS.BUTTONSIZE, obj.DEFS.BUTTONSIZE]));
                    end
                end
            end

            % Position shared controls using the middle of the stack
            midXAll = left + width/2;
            topAxisTop = bottomBase + heightAvail;

            if ~isempty(obj.splitButton) && isvalid(obj.splitButton)
                set(obj.splitButton, 'Position', max(1, [midXAll + obj.DEFS.BIGBUTTONSIZE, ...
                    obj.DEFS.PADDING, obj.DEFS.BIGBUTTONSIZE, obj.DEFS.BUTTONSIZE]));
            end

            if ~isempty(obj.chbuts)
                for i = 1:numel(obj.chbuts)
                    set(obj.chbuts(i),'Position', ...
                        max(1,[midXAll - (numel(obj.chbuts)/2 - i + 0.5)*obj.DEFS.BIGBUTTONSIZE, ...
                        obj.DEFS.PADDING + obj.DEFS.BUTTONSIZE + obj.DEFS.PADDING, ...
                        obj.DEFS.BIGBUTTONSIZE, ...
                        obj.DEFS.BUTTONSIZE]));
                end
            end

            for i=1:numel(obj.tbuts)
                set(obj.tbuts(i),'Position', ...
                    max(1,[midXAll+(i-numel(obj.tbuts)/2-1)*obj.DEFS.BIGBUTTONSIZE, ...
                    topAxisTop + obj.DEFS.PADDING, ...
                    obj.DEFS.BIGBUTTONSIZE, ...
                    obj.DEFS.BIGBUTTONSIZE]));
            end
        end

        function clearSplitView(obj)
            % Delete split axes and controls
            if ~isempty(obj.subplotAxes)
                delete(obj.subplotAxes(isgraphics(obj.subplotAxes)));
            end
            for idx = 1:numel(obj.splitYButtons)
                delete(obj.splitYButtons{idx}(isgraphics(obj.splitYButtons{idx})));
            end
            for idx = 1:numel(obj.splitXButtons)
                delete(obj.splitXButtons{idx}(isgraphics(obj.splitXButtons{idx})));
            end

            obj.subplotAxes = gobjects(1,0);
            obj.subplotCaches = {};
            obj.splitYButtons = {};
            obj.splitXButtons = {};
        end

        function updateSplitCaches(obj, processedData)
            if ~obj.splitMode || isempty(obj.subplotCaches)
                return;
            end

            visibleChannels = find(obj.channelVisible);
            for idx = 1:numel(obj.subplotCaches)
                cache = obj.subplotCaches{idx};
                if isempty(cache) || ~isgraphics(cache.ax)
                    continue;
                end

                channelIdx = visibleChannels(idx);
                if channelIdx <= size(processedData, 2) - 1
                    channelData = processedData(:, [1, channelIdx + 1]);
                    cache.update_cache(channelData);
                    cache.draw_fig_now();
                end
            end
        end

        function noiseMode(obj)
            % stop other things that might have been running
            obj.stopAll;
            obj.mode = 'noise';

            % set default positions
            obj.noiseSizing;
            obj.noiseResizeFcn;

            % No figure cache necessary
            obj.fig_cache = [];

            % Show figure
            obj.fig.Visible = 'on';

        end

        function noiseSizing(obj)
            delete(obj.panel);
            obj.panel = uipanel('Parent',obj.fig,'Position',[0 0 1 1]);

            delete(obj.axes);
            obj.axes = axes('Parent',obj.panel,'Position',[0.05 0.05 0.9 0.9],...
                'GridLineStyle','-','XColor', 0.15*[1 1 1],'YColor', 0.15*[1 1 1]);
            set(obj.axes,'NextPlot','add','XLimMode','manual');
            set(obj.axes,'XGrid','on','YGrid','on','Tag','Axes', ...
                'Box','on','XScale','log','YScale','log');
            obj.axes.YLabel.String = 'Current noise power spectral density (pA^2/Hz)';
            obj.axes.YLabel.Color = 'k';
            obj.axes.XLabel.String = 'Frequency (Hz)';
            obj.axes.XLabel.Color = 'k';
            obj.axes.YLim = [1e-8 1e2];
            obj.axes.XLim = [1 5e4];

            % now make the buttons

            % Use plain-text labels instead of HTML for MATLAB UI compatibility
            zoomOutLabel = '-';
            scrollDownLabel = char(8595); % down arrow
            resetLabel = 'R';
            scrollUpLabel = char(8593);   % up arrow
            zoomInLabel = '+';

            % y-axis
            obj.ybuts = [];
            obj.ybuts(1) = uicontrol('Parent', obj.panel, 'String', zoomOutLabel,...
                'callback', @(~,~) zoom_y('out'));
            obj.ybuts(2) = uicontrol('Parent', obj.panel, 'String', scrollDownLabel,...
                'callback', @(~,~) scroll_y('down'));
            obj.ybuts(3) = uicontrol('Parent', obj.panel, 'String', resetLabel,...
                'callback', @(~,~) reset_fig);
            obj.ybuts(4) = uicontrol('Parent', obj.panel, 'String', scrollUpLabel,...
                'callback', @(~,~) scroll_y('up'));
            obj.ybuts(5) = uicontrol('Parent', obj.panel, 'String', zoomInLabel,...
                'callback', @(~,~) zoom_y('in'));

            function zoom_y(str)
                if strcmp(str,'in')
                    obj.axes.YLim = 10.^(log10(get(obj.axes,'YLim')) + [1 -1]);
                elseif strcmp(str,'out')
                    obj.axes.YLim = 10.^(log10(get(obj.axes,'YLim')) + [-1 1]);
                end
            end

            function scroll_y(str)
                if strcmp(str,'up')
                    obj.axes.YLim = 10.^(log10(get(obj.axes,'YLim')) + [1 1]);
                elseif strcmp(str,'down')
                    obj.axes.YLim = 10.^(log10(get(obj.axes,'YLim')) + [-1 -1]);
                end
            end

            function reset_fig
                obj.axes.YLim = [1e-8 1e2];
                obj.axes.XLim = [1 5e4];
            end

            % Channel toggle buttons - create for all 8 channels
            obj.chbuts = [];
            
            % Get MATLAB default color order (same as used in plots)
            colorOrder = get(groot,'defaultaxescolororder');
            % Extend to 8 colors if needed
            if size(colorOrder, 1) < 8
                colorOrder = repmat(colorOrder, ceil(8/size(colorOrder,1)), 1);
            end
            
            % Define color scheme
            disabledColor = [0.7 0.7 0.7];   % Light gray for disabled
            diffColor = [0.1 0.5 0.9];       % Blue for differential
            
            for i = 1:8
                channelNum = i - 1; % Channel 0-7
                isEnabled = obj.channelEnabled(i);
                
                % Use plot color for enabled channels, gray for disabled
                if isEnabled
                    bgColor = colorOrder(i, :);
                else
                    bgColor = disabledColor;
                end
                
                obj.chbuts(i) = uicontrol('Parent', obj.panel, ...
                    'Style', 'togglebutton', 'String', ['CH' num2str(channelNum)], ...
                    'FontSize', 8, 'FontWeight', 'bold', ...
                    'ForegroundColor', [1 1 1], ...
                    'BackgroundColor', bgColor, ...
                    'callback', @(src,~) obj.toggleChannel(src, i), ...
                    'Value', isEnabled, 'tag', ['channel_' num2str(i)], ...
                    'UserData', struct('isDifferential', false, 'pairChannel', -1, ...
                                      'channelColor', colorOrder(i, :), 'disabledColor', disabledColor, 'diffColor', diffColor));
                
                % Add context menu for differential mode (only for channels 0-3)
                if channelNum < 4
                    cmenu = uicontextmenu;
                    uimenu(cmenu, 'Label', ['Toggle Differential ' num2str(channelNum) '-' num2str(channelNum+4)], ...
                        'Callback', @(~,~) obj.toggleDifferentialPair(i));
                    set(obj.chbuts(i), 'UIContextMenu', cmenu);
                end
            end

            % top
            obj.tbuts = [];
            obj.tbuts(1) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Play.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'noise');
            %obj.tbuts(2) = uicontrol('Parent', obj.panel, ...
            %    'Style', 'togglebutton', 'CData', imread('Voltage_Off.png'),...
            %    'callback', @(src,~) obj.stateDecision(src), 'tag', 'set');

            % set the resize function
            set(obj.panel, 'ResizeFcn', @(~,~) obj.noiseResizeFcn);
            % and call it to set default positions
            obj.noiseResizeFcn;

            % Update channel display
            obj.updateChannelDisplay();

            % Show figure
            obj.fig.Visible = 'on';

        end

        function noiseResizeFcn(obj)
            % get size of panel in pixels
            sz = obj.getPixelPos(obj.panel);
            % position the axes object
            sz(1) = sz(1) + obj.DEFS.PADDING + obj.DEFS.LABELWIDTH + obj.DEFS.BUTTONSIZE; % left
            sz(3) = sz(3) - sz(1); % width
            sz(2) = sz(2) + obj.DEFS.PADDING + obj.DEFS.LABELWIDTH; % bottom
            sz(4) = sz(4) - sz(2) - obj.DEFS.BIGBUTTONSIZE - 3*obj.DEFS.PADDING; % height
            set(obj.axes,'Position',max(1,sz),'Units','Pixels');
            % get size of axes in pixels
            sz = obj.getPixelPos(obj.axes);
            % figure out where the y middle is
            midle = sz(4)/2 + sz(2);
            % position the buttons
            for i=1:numel(obj.ybuts)
                set(obj.ybuts(i),'Position',...
                    max(1,[obj.DEFS.PADDING, ...
                    midle+(i-numel(obj.ybuts)/2-1)*obj.DEFS.BUTTONSIZE, ...
                    obj.DEFS.BUTTONSIZE, ...
                    obj.DEFS.BUTTONSIZE]));
            end
            % figure out where the x middle is
            midle = sz(3)/2 + sz(1);
            % position the buttons
            for i=1:numel(obj.tbuts)
                set(obj.tbuts(i),'Position', ...
                    max(1,[midle+(i-numel(obj.tbuts)/2-1)*obj.DEFS.BIGBUTTONSIZE, ...
                    sz(2) + sz(4) + obj.DEFS.PADDING, ...
                    obj.DEFS.BIGBUTTONSIZE, ...
                    obj.DEFS.BIGBUTTONSIZE]));
            end
            % position the channel toggle buttons (above x-axis buttons)
            if ~isempty(obj.chbuts)
                for i=1:numel(obj.chbuts)
                    set(obj.chbuts(i),'Position', ...
                        max(1,[midle - (numel(obj.chbuts)/2 - i + 0.5)*obj.DEFS.BIGBUTTONSIZE, ...
                        obj.DEFS.PADDING + obj.DEFS.BUTTONSIZE + obj.DEFS.PADDING, ...
                        obj.DEFS.BIGBUTTONSIZE, ...
                        obj.DEFS.BUTTONSIZE]));
                end
            end
        end

        function ivMode(obj)
            % stop other things that might have been running
            obj.stopAll;
            obj.mode = 'iv';

            % set default positions
            obj.ivSizing;
            obj.ivResizeFcn;

            % No figure cache necessary
            obj.fig_cache = [];

            % Show figure
            obj.fig.Visible = 'on';

        end

        function ivSizing(obj)
            delete(obj.panel);
            obj.panel = uipanel('Parent',obj.fig,'Position',[0 0 1 1]);

            delete(obj.axes);
            obj.axes(1) = axes('Parent',obj.panel,'Position',[0.05 0.05 0.45 0.90],...
                'GridLineStyle','-','XColor', 0.15*[1 1 1],'YColor', 0.15*[1 1 1]);
            set(obj.axes(1),'NextPlot','add','XLimMode','manual');
            set(obj.axes(1),'XGrid','on','YGrid','on','Tag','Axes','Box','on');
            obj.axes(1).YLabel.String = 'Current (pA)';
            obj.axes(1).YLabel.Color = 'k';
            obj.axes(1).XLabel.String = 'Time (s)';
            obj.axes(1).XLabel.Color = 'k';
            obj.axes(1).YLim = [-800 800];
            obj.axes(1).XLim = [0 0.8];

            obj.axes(2) = axes('Parent',obj.panel,'Position',[0.55 0.05 0.45 0.90],...
                'GridLineStyle','-','XColor', 0.15*[1 1 1],'YColor', 0.15*[1 1 1]);
            set(obj.axes(2),'NextPlot','add','XLimMode','manual');
            set(obj.axes(2),'XGrid','on','YGrid','on','Tag','Axes','Box','on');
            obj.axes(2).YLabel.String = 'Mean Current (pA)';
            obj.axes(2).YLabel.Color = 'k';
            obj.axes(2).XLabel.String = 'Voltage (mV)';
            obj.axes(2).XLabel.Color = 'k';
            obj.axes(2).YLim = [-800 800];
            obj.axes(2).XLim = [-200 200];

            % now make the buttons

            % Use plain-text labels instead of HTML for MATLAB UI compatibility
            zoomOutLabel = '-';
            scrollDownLabel = char(8595); % down arrow
            resetLabel = 'R';
            scrollUpLabel = char(8593);   % up arrow
            zoomInLabel = '+';

            % y-axis
            obj.ybuts = [];
            obj.ybuts(1) = uicontrol('Parent', obj.panel, 'String', zoomOutLabel,...
                'callback', @(~,~) zoom_y('out'));
            obj.ybuts(2) = uicontrol('Parent', obj.panel, 'String', scrollDownLabel,...
                'callback', @(~,~) scroll_y('down'));
            obj.ybuts(3) = uicontrol('Parent', obj.panel, 'String', resetLabel,...
                'callback', @(~,~) reset_fig);
            obj.ybuts(4) = uicontrol('Parent', obj.panel, 'String', scrollUpLabel,...
                'callback', @(~,~) scroll_y('up'));
            obj.ybuts(5) = uicontrol('Parent', obj.panel, 'String', zoomInLabel,...
                'callback', @(~,~) zoom_y('in'));

            function zoom_y(str)
                if strcmp(str,'in')
                    lims = 1/2*get(obj.axes(1),'YLim');
                elseif strcmp(str,'out')
                    lims = 2*get(obj.axes(1),'YLim');
                end
                obj.axes(1).YLim = lims;
                obj.axes(2).YLim = lims;
            end

            function scroll_y(str)
                if strcmp(str,'up')
                    lims = get(obj.axes(1),'YLim') + [1 1]*diff(get(obj.axes(1),'YLim'))/5;
                elseif strcmp(str,'down')
                    lims = get(obj.axes(1),'YLim') - [1 1]*diff(get(obj.axes(1),'YLim'))/5;
                end
                obj.axes(1).YLim = lims;
                obj.axes(2).YLim = lims;
            end

            function reset_fig
                obj.axes(1).YLim = [-800 800];
                obj.axes(2).YLim = [-800 800];
                obj.axes(1).XLim = [0 0.8];
            end

            % top
            obj.tbuts = [];
            obj.tbuts(1) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Sweep_Up.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'iv_sweep_up');
            obj.tbuts(2) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Sweep_Bi.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'iv_sweep_bi');
            obj.tbuts(3) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Sweep_Down.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'iv_sweep_down');
            % set the resize function
            set(obj.panel, 'ResizeFcn', @(~,~) obj.ivResizeFcn);
            % and call it to set default positions
            obj.ivResizeFcn;

            % Show figure
            obj.fig.Visible = 'on';

        end

        function ivResizeFcn(obj)
            % position the axes1 object
            sz = obj.getPixelPos(obj.panel);
            sz(1) = sz(1) + obj.DEFS.PADDING + obj.DEFS.LABELWIDTH + obj.DEFS.BUTTONSIZE; % left
            sz(3) = sz(3)/2 - obj.DEFS.PADDING - 1.5*obj.DEFS.LABELWIDTH - obj.DEFS.BUTTONSIZE - 2*obj.DEFS.PADDING; % width
            sz(2) = sz(2) + obj.DEFS.PADDING + obj.DEFS.LABELWIDTH; % bottom
            sz(4) = sz(4) - sz(2) - obj.DEFS.BIGBUTTONSIZE - 3*obj.DEFS.PADDING; % height
            set(obj.axes(1),'Position',max(1,sz),'Units','Pixels');
            set(obj.axes(1),'Position',max(1,sz),'Units','Pixels');
            % figure out where the y middle is
            midle = sz(4)/2 + sz(2);
            % position the buttons
            for i=1:numel(obj.ybuts)
                set(obj.ybuts(i),'Position', ...
                    [obj.DEFS.PADDING, ...
                    midle+(i-numel(obj.ybuts)/2-1)*obj.DEFS.BUTTONSIZE, ...
                    obj.DEFS.BUTTONSIZE, ...
                    obj.DEFS.BUTTONSIZE]);
            end
            % position the axes2 object
            sz = obj.getPixelPos(obj.panel);
            sz(1) = sz(1) + sz(3)/2 + obj.DEFS.PADDING + obj.DEFS.LABELWIDTH + obj.DEFS.BUTTONSIZE; % left
            sz(3) = sz(3)/2 - obj.DEFS.PADDING - 1.5*obj.DEFS.LABELWIDTH - obj.DEFS.BUTTONSIZE - 2*obj.DEFS.PADDING; % width
            sz(2) = sz(2) + obj.DEFS.PADDING + obj.DEFS.LABELWIDTH; % bottom
            sz(4) = sz(4) - sz(2) - obj.DEFS.BIGBUTTONSIZE - 3*obj.DEFS.PADDING; % height
            set(obj.axes(2),'Position',max(1,sz),'Units','Pixels');
            % position the top button
            bottom = sz(2) + sz(4) + obj.DEFS.PADDING;
            sz = obj.getPixelPos(obj.panel);
            midle = sz(1)+sz(3)/2;
            for i=1:numel(obj.tbuts)
                set(obj.tbuts(i),'Position', ...
                    max(1,[midle+(i-numel(obj.tbuts)/2-1)*obj.DEFS.BIGBUTTONSIZE, ...
                    bottom, ...
                    obj.DEFS.BIGBUTTONSIZE, ...
                    obj.DEFS.BIGBUTTONSIZE]));
            end
        end

        function sealtestMode(obj)
            % stop other things that might have been running
            obj.stopAll;
            obj.mode = 'sealtest';

            % set default positions
            obj.sealtestSizing;
            obj.sealtestResizeFcn;

            % no figure cache
            obj.fig_cache = [];

            % Show figure
            obj.fig.Visible = 'on';

        end

        function sealtestSizing(obj)
            delete(obj.panel);
            obj.panel = uipanel('Parent',obj.fig,'Position',[0 0 1 1]);

            delete(obj.axes);
            obj.axes = axes('Parent',obj.panel,'Position',[0.05 0.05 0.95 0.95],...
                'GridLineStyle','-','XColor', 0.15*[1 1 1],'YColor', 0.15*[1 1 1]);
            set(obj.axes,'NextPlot','replacechildren','XLimMode','manual');
            set(obj.axes,'XGrid','on','YGrid','on','Tag','Axes','Box','on');
            obj.axes.YLabel.String = 'Current (pA)';
            obj.axes.YLabel.Color = 'k';
            obj.axes.XLabel.String = 'Time (s)';
            obj.axes.XLabel.Color = 'k';
            obj.axes.YLim = [-800, 800];
            obj.axes.XLim = [0, 0.05];

            % now make the buttons

            % Use plain-text labels instead of HTML for MATLAB UI compatibility
            zoomOutLabel = '-';
            scrollDownLabel = char(8595); % down arrow
            resetLabel = 'R';
            scrollUpLabel = char(8593);   % up arrow
            zoomInLabel = '+';

            % y-axis
            obj.ybuts = [];
            obj.ybuts(1) = uicontrol('Parent', obj.panel, 'String', zoomOutLabel,...
                'callback', @(~,~) zoom_y('out'));
            obj.ybuts(2) = uicontrol('Parent', obj.panel, 'String', scrollDownLabel,...
                'callback', @(~,~) scroll_y('down'));
            obj.ybuts(3) = uicontrol('Parent', obj.panel, 'String', resetLabel,...
                'callback', @(~,~) reset_fig);
            obj.ybuts(4) = uicontrol('Parent', obj.panel, 'String', scrollUpLabel,...
                'callback', @(~,~) scroll_y('up'));
            obj.ybuts(5) = uicontrol('Parent', obj.panel, 'String', zoomInLabel,...
                'callback', @(~,~) zoom_y('in'));

            function zoom_y(str)
                if strcmp(str,'in')
                    obj.axes.YLim = 1/2*get(obj.axes(1),'YLim');
                elseif strcmp(str,'out')
                    obj.axes.YLim = 2*get(obj.axes(1),'YLim');
                end
            end

            function scroll_y(str)
                if strcmp(str,'up')
                    obj.axes.YLim = get(obj.axes(1),'YLim') + [1 1]*diff(get(obj.axes(1),'YLim'))/5;
                elseif strcmp(str,'down')
                    obj.axes.YLim = get(obj.axes(1),'YLim') - [1 1]*diff(get(obj.axes(1),'YLim'))/5;
                end
            end

            function reset_fig
                obj.axes.XLim = [0, 0.1];
                obj.axes.YLim = [-800, 800];
            end

            % top
            obj.tbuts = [];
            obj.tbuts(1) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Record.png'), ...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'sealtest_record');
            obj.tbuts(2) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Play.png'), ...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'sealtest_play');
            % set the resize function
            set(obj.panel, 'ResizeFcn', @(~,~) obj.sealtestResizeFcn);
            % and call it to set default positions
            obj.sealtestResizeFcn;

            % Show figure
            obj.fig.Visible = 'on';

        end

        function sealtestResizeFcn(obj)
            % get size of panel in pixels
            sz = obj.getPixelPos(obj.panel);
            % position the axes object
            sz(1) = sz(1) + obj.DEFS.PADDING + obj.DEFS.LABELWIDTH + obj.DEFS.BUTTONSIZE; % left
            sz(3) = sz(3) - sz(1); % width
            sz(2) = sz(2) + obj.DEFS.PADDING + obj.DEFS.LABELWIDTH; % bottom
            sz(4) = sz(4) - sz(2) - obj.DEFS.BIGBUTTONSIZE - 3*obj.DEFS.PADDING; % height
            set(obj.axes,'Position',max(1,sz),'Units','Pixels');
            % get size of axes in pixels
            sz = obj.getPixelPos(obj.axes);
            % figure out where the y middle is
            midle = sz(4)/2 + sz(2);
            % position the buttons
            for i=1:numel(obj.ybuts)
                set(obj.ybuts(i),'Position', ...
                    max(1,[obj.DEFS.PADDING, ...
                    midle+(i-numel(obj.ybuts)/2-1)*obj.DEFS.BUTTONSIZE, ...
                    obj.DEFS.BUTTONSIZE, ...
                    obj.DEFS.BUTTONSIZE]));
            end
            % figure out where the x middle is
            midle = sz(3)/2 + sz(1);
            % position the buttons
            for i=1:numel(obj.tbuts)
                set(obj.tbuts(i),'Position', ...
                    max(1,[midle+(i-numel(obj.tbuts)/2-1)*obj.DEFS.BIGBUTTONSIZE, ...
                    sz(2) + sz(4) + obj.DEFS.PADDING, ...
                    obj.DEFS.BIGBUTTONSIZE, ...
                    obj.DEFS.BIGBUTTONSIZE]));
            end
        end

        function freqrespMode(obj)
            % stop other things that might have been running
            obj.stopAll;
            obj.mode = 'fra';

            % set default positions
            obj.freqrespSizing;
            obj.freqrespResizeFcn;

            % No figure cache necessary
            obj.fig_cache = [];

            % Show figure
            obj.fig.Visible = 'on';

        end

        function freqrespSizing(obj)
            delete(obj.panel);
            obj.panel = uipanel('Parent',obj.fig,'Position',[0 0 1 1]);

            delete(obj.axes);
            obj.axes(1) = axes('Parent',obj.panel,'Position',[0.05 0.05 0.45 0.90],...
                'GridLineStyle','-','XColor', 0.15*[1 1 1],'YColor', 0.15*[1 1 1]);
            set(obj.axes(1),'NextPlot','add','XLimMode','manual');
            set(obj.axes(1),'XGrid','on','YGrid','on','Tag','Axes','Box','on');
            obj.axes(1).YLabel.String = 'Voltage (V)';
            obj.axes(1).YLabel.Color = 'k';
            obj.axes(1).XLabel.String = 'Time (s)';
            obj.axes(1).XLabel.Color = 'k';
            obj.axes(1).YLim = [-0.2 0.2];
            obj.axes(1).XLim = [0 0.005];

            obj.axes(2) = axes('Parent',obj.panel,'Position',[0.55 0.05 0.45 0.90],...
                'GridLineStyle','-','XColor', 0.15*[1 1 1],'YColor', 0.15*[1 1 1]);
            set(obj.axes(2),'NextPlot','add','XLimMode','manual');
            set(obj.axes(2),'XGrid','on','YGrid','on','Tag','Axes','Box','on','XScale','log');
            obj.axes(2).YLabel.String = 'Gain (dB)';
            obj.axes(2).YLabel.Color = 'k';
            obj.axes(2).XLabel.String = 'Frequency (Hz)';
            obj.axes(2).XLabel.Color = 'k';
            obj.axes(2).YLim = [-120 60];
            obj.axes(2).XLim = [1 5e4];

            % now make the buttons

            % Use plain-text labels instead of HTML for MATLAB UI compatibility
            zoomOutLabel = '-';
            scrollDownLabel = char(8595); % down arrow
            resetLabel = 'R';
            scrollUpLabel = char(8593);   % up arrow
            zoomInLabel = '+';

            % y-axis
            obj.ybuts = [];
            obj.ybuts(1) = uicontrol('Parent', obj.panel, 'String', zoomOutLabel,...
                'callback', @(~,~) zoom_y('out'));
            obj.ybuts(2) = uicontrol('Parent', obj.panel, 'String', scrollDownLabel,...
                'callback', @(~,~) scroll_y('down'));
            obj.ybuts(3) = uicontrol('Parent', obj.panel, 'String', resetLabel,...
                'callback', @(~,~) reset_fig);
            obj.ybuts(4) = uicontrol('Parent', obj.panel, 'String', scrollUpLabel,...
                'callback', @(~,~) scroll_y('up'));
            obj.ybuts(5) = uicontrol('Parent', obj.panel, 'String', zoomInLabel,...
                'callback', @(~,~) zoom_y('in'));

            function zoom_y(str)
                if strcmp(str,'in')
                    lims = 1/2*get(obj.axes(1),'YLim');
                elseif strcmp(str,'out')
                    lims = 2*get(obj.axes(1),'YLim');
                end
                obj.axes(1).YLim = lims;
                %obj.axes(2).YLim = lims;
            end

            function scroll_y(str)
                if strcmp(str,'up')
                    lims = get(obj.axes(1),'YLim') + [1 1]*diff(get(obj.axes(1),'YLim'))/5;
                elseif strcmp(str,'down')
                    lims = get(obj.axes(1),'YLim') - [1 1]*diff(get(obj.axes(1),'YLim'))/5;
                end
                obj.axes(1).YLim = lims;
                %obj.axes(2).YLim = lims;
            end

            function reset_fig
                obj.axes(1).YLim = [-0.2 0.2];
                obj.axes(2).YLim = [-120 60];
                obj.axes(1).XLim = [0 0.1];
                obj.axes(2).YLim = [1 5e4];
            end

            % top
            obj.tbuts = [];
            obj.tbuts(1) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Record.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'freqresp_record');
            obj.tbuts(2) = uicontrol('Parent', obj.panel, ...
                'Style', 'togglebutton', 'CData', imread('Play.png'),...
                'callback', @(src,~) obj.stateDecision(src), 'tag', 'freqresp_play');
            % set the resize function
            set(obj.panel, 'ResizeFcn', @(~,~) obj.freqrespResizeFcn);
            % and call it to set default positions
            obj.freqrespResizeFcn;

            % Show figure
            obj.fig.Visible = 'on';

        end

        function freqrespResizeFcn(obj)
            % position the axes1 object
            sz = obj.getPixelPos(obj.panel);
            sz(1) = sz(1) + obj.DEFS.PADDING + obj.DEFS.LABELWIDTH + obj.DEFS.BUTTONSIZE; % left
            sz(3) = sz(3)/2 - obj.DEFS.PADDING - 1.5*obj.DEFS.LABELWIDTH - obj.DEFS.BUTTONSIZE - 2*obj.DEFS.PADDING; % width
            sz(2) = sz(2) + obj.DEFS.PADDING + obj.DEFS.LABELWIDTH; % bottom
            sz(4) = sz(4) - sz(2) - obj.DEFS.BIGBUTTONSIZE - 3*obj.DEFS.PADDING; % height
            set(obj.axes(1),'Position',max(1,sz),'Units','Pixels');
            set(obj.axes(1),'Position',max(1,sz),'Units','Pixels');
            % figure out where the y middle is
            midle = sz(4)/2 + sz(2);
            % position the buttons
            for i=1:numel(obj.ybuts)
                set(obj.ybuts(i),'Position', ...
                    [obj.DEFS.PADDING, ...
                    midle+(i-numel(obj.ybuts)/2-1)*obj.DEFS.BUTTONSIZE, ...
                    obj.DEFS.BUTTONSIZE, ...
                    obj.DEFS.BUTTONSIZE]);
            end
            % position the axes2 object
            sz = obj.getPixelPos(obj.panel);
            sz(1) = sz(1) + sz(3)/2 + obj.DEFS.PADDING + obj.DEFS.LABELWIDTH + obj.DEFS.BUTTONSIZE; % left
            sz(3) = sz(3)/2 - obj.DEFS.PADDING - 1.5*obj.DEFS.LABELWIDTH - obj.DEFS.BUTTONSIZE - 2*obj.DEFS.PADDING; % width
            sz(2) = sz(2) + obj.DEFS.PADDING + obj.DEFS.LABELWIDTH; % bottom
            sz(4) = sz(4) - sz(2) - obj.DEFS.BIGBUTTONSIZE - 3*obj.DEFS.PADDING; % height
            set(obj.axes(2),'Position',max(1,sz),'Units','Pixels');
            % position the top button
            bottom = sz(2) + sz(4) + obj.DEFS.PADDING;
            sz = obj.getPixelPos(obj.panel);
            midle = sz(1)+sz(3)/2;
            for i=1:numel(obj.tbuts)
                set(obj.tbuts(i),'Position', ...
                    max(1,[midle+(i-numel(obj.tbuts)/2-1)*obj.DEFS.BIGBUTTONSIZE, ...
                    bottom, ...
                    obj.DEFS.BIGBUTTONSIZE, ...
                    obj.DEFS.BIGBUTTONSIZE]));
            end
        end
        
        function writeFileHeader(obj, fid)
            % write a file header
            h.si = 1/obj.sampling;
            
            % Generate channel names and determine which channels to save
            enabledIndices = find(obj.channelEnabled);
            channelNames = {};
            saveChannelFlags = false(1, 8);
            scalingFactors = [];
            
            % Track which channels have been processed as differential
            processedChannels = false(1, 8);
            
            for i = 1:length(enabledIndices)
                chanIdx = enabledIndices(i);  % 1-based index
                chanNum = chanIdx - 1;        % 0-based channel number
                
                if processedChannels(chanIdx)
                    continue;
                end
                
                userData = get(obj.chbuts(chanIdx), 'UserData');
                
                if ~isempty(userData) && isfield(userData, 'isDifferential') && userData.isDifferential && userData.pairChannel > 0
                    % This is a differential pair
                    lowIdx = chanIdx;
                    highIdx = userData.pairChannel;
                    lowNum = lowIdx - 1;
                    highNum = highIdx - 1;
                    
                    % Add differential channel name
                    channelNames{end+1} = sprintf('CH%d-%d (V)', lowNum, highNum);
                    scalingFactors(end+1) = obj.data_compression_scaling(lowIdx);
                    saveChannelFlags(lowIdx) = true;  % Save differential result on low channel
                    
                    % Mark both channels as processed
                    processedChannels(lowIdx) = true;
                    processedChannels(highIdx) = true;
                else
                    % Single-ended channel
                    channelNames{end+1} = sprintf('CH%d (V)', chanNum);
                    scalingFactors(end+1) = obj.data_compression_scaling(chanIdx);
                    saveChannelFlags(chanIdx) = true;
                    processedChannels(chanIdx) = true;
                end
            end
            
            h.chNames = channelNames;
            h.data_compression_scaling = scalingFactors;
            obj.saveChannelFlags = saveChannelFlags;  % Store in object for writeFileData
            
            hh = getByteStreamFromArray(h);
            n = numel(hh);
            fwrite(fid,n,'*uint32');
            fwrite(fid,hh,'*uint8');
        end

        function writeFileData(obj, fid, data)
            % Only write data for channels that should be saved
            % In differential mode, only save the differential result (on low channel)
            
            % Use the save flags stored in the object
            if isfield(obj, 'saveChannelFlags')
                saveFlags = obj.saveChannelFlags;
            else
                % Fallback to enabled channels if save flags not available
                saveFlags = obj.channelEnabled;
            end
            
            % Only write data for channels marked for saving
            enabledData = data(saveFlags, :);
            enabledScaling = obj.data_compression_scaling(saveFlags);
            fwrite(fid,int16(repmat(enabledScaling(:),1,size(enabledData,2)) .* enabledData),'*int16');
        end

        function stateDecision(obj, src)
            % state machine for the main button presses
            if strcmp(get(src,'tag'),'play')
                button_state = get(src,'Value');
                if button_state == get(src,'Max')
                    % we are in play mode
                    recbutton = findobj(obj.tbuts,'tag','record');
                    obj.stopRecord(recbutton);
                    set(recbutton,'Value',0);
                    playbutton = findobj(obj.tbuts,'tag','play');
                    obj.play(src);
                    set(playbutton,'Value',1);
                else
                    % we are stopped
                    playbutton = findobj(obj.tbuts,'tag','play');
                    obj.stopPlay(src);
                    set(playbutton,'Value',0);
                end
            elseif strcmp(get(src,'tag'),'record')
                button_state = get(src,'Value');
                if button_state == get(src,'Max')
                    % we are in record mode
                    playbutton = findobj(obj.tbuts,'tag','play');
                    obj.stopPlay(playbutton);
                    set(playbutton,'Value',0);
                    recbutton = findobj(obj.tbuts,'tag','record');
                    obj.record(src);
                    set(recbutton,'Value',1);
                else
                    % we are stopped
                    recbutton = findobj(obj.tbuts,'tag','record');
                    obj.stopRecord(src);
                    set(recbutton,'Value',0);
                end
            elseif strcmp(get(src,'tag'),'set_negative_higher_voltage')
                if get(findobj(obj.tbuts,'tag','record'),'Value') || get(findobj(obj.tbuts,'tag','play'),'Value')
                    button_state = get(src,'Value');
                    if button_state == get(src,'Max')
                        offbutton = findobj(obj.tbuts,'tag','set_negative_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',1);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_high_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_low_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_low_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_high_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);

                        % we are in setVoltage mode
                        obj.setNegativeHigherVoltage(src);
                    else
                        % we are resetted
                        obj.resetVoltage(src);
                    end
                end
            elseif strcmp(get(src,'tag'),'set_negative_high_voltage')
                if get(findobj(obj.tbuts,'tag','record'),'Value') || get(findobj(obj.tbuts,'tag','play'),'Value')
                    button_state = get(src,'Value');
                    if button_state == get(src,'Max')
                        offbutton = findobj(obj.tbuts,'tag','set_negative_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_high_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',1);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_low_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_low_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_high_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        % we are in setVoltage mode
                        obj.setNegativeHighVoltage(src);
                    else
                        % we are resetted
                        obj.resetVoltage(src);
                    end
                end
            elseif strcmp(get(src,'tag'),'set_negative_midtwo_voltage')
                if get(findobj(obj.tbuts,'tag','record'),'Value') || get(findobj(obj.tbuts,'tag','play'),'Value')
                    button_state = get(src,'Value');
                    if button_state == get(src,'Max')
                        offbutton = findobj(obj.tbuts,'tag','set_negative_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_high_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',1);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_low_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_low_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_high_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        % we are in setVoltage mode
                        obj.setNegativeMidtwoVoltage(src);
                    else
                        % we are resetted
                        obj.resetVoltage(src);
                    end
                end
            elseif strcmp(get(src,'tag'),'set_negative_mid_voltage')
                if get(findobj(obj.tbuts,'tag','record'),'Value') || get(findobj(obj.tbuts,'tag','play'),'Value')
                    button_state = get(src,'Value');
                    if button_state == get(src,'Max')
                        offbutton = findobj(obj.tbuts,'tag','set_negative_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_high_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',1);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_low_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_low_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_high_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        % we are in setVoltage mode
                        obj.setNegativeMidVoltage(src);
                    else
                        % we are resetted
                        obj.resetVoltage(src);
                    end
                end
            elseif strcmp(get(src,'tag'),'set_negative_low_voltage')
                if get(findobj(obj.tbuts,'tag','record'),'Value') || get(findobj(obj.tbuts,'tag','play'),'Value')
                    button_state = get(src,'Value');
                    if button_state == get(src,'Max')
                        offbutton = findobj(obj.tbuts,'tag','set_negative_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_high_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_low_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',1);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_low_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_high_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        % we are in setVoltage mode
                        obj.setNegativeLowVoltage(src);
                    else
                        % we are resetted
                        obj.resetVoltage(src);
                    end
                end
            elseif strcmp(get(src,'tag'),'set_positive_low_voltage')
                if get(findobj(obj.tbuts,'tag','record'),'Value') ||get(findobj(obj.tbuts,'tag','play'),'Value')
                    button_state = get(src,'Value');
                    if button_state == get(src,'Max')
                        offbutton = findobj(obj.tbuts,'tag','set_negative_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_high_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_low_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_low_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',1);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_high_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        % we are in setVoltage mode
                        obj.setPositiveLowVoltage(src);
                    else
                        % we are resetted
                        obj.resetVoltage(src);
                    end
                end
            elseif strcmp(get(src,'tag'),'set_positive_mid_voltage')
                if get(findobj(obj.tbuts,'tag','record'),'Value') || get(findobj(obj.tbuts,'tag','play'),'Value')
                    button_state = get(src,'Value');
                    if button_state == get(src,'Max')
                        offbutton = findobj(obj.tbuts,'tag','set_negative_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_high_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_low_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_low_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',1);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_high_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        % we are in setVoltage mode
                        obj.setPositiveMidVoltage(src);
                    else
                        % we are resetted
                        obj.resetVoltage(src);
                    end
                end
            elseif strcmp(get(src,'tag'),'set_positive_midtwo_voltage')
                if get(findobj(obj.tbuts,'tag','record'),'Value') || get(findobj(obj.tbuts,'tag','play'),'Value')
                    button_state = get(src,'Value');
                    if button_state == get(src,'Max')
                        offbutton = findobj(obj.tbuts,'tag','set_negative_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_high_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_low_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_low_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',1);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_high_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        % we are in setVoltage mode
                        obj.setPositiveMidtwoVoltage(src);
                    else
                        % we are resetted
                        obj.resetVoltage(src);
                    end
                end
            elseif strcmp(get(src,'tag'),'set_positive_high_voltage')
                if get(findobj(obj.tbuts,'tag','record'),'Value') || get(findobj(obj.tbuts,'tag','play'),'Value')
                    button_state = get(src,'Value');
                    if button_state == get(src,'Max')
                        offbutton = findobj(obj.tbuts,'tag','set_negative_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_high_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_low_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_low_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_high_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',1);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        % we are in setVoltage mode
                        obj.setPositiveHighVoltage(src);
                    else
                        % we are resetted
                        obj.resetVoltage(src);
                    end
                end
            elseif strcmp(get(src,'tag'),'set_positive_higher_voltage')
                if get(findobj(obj.tbuts,'tag','record'),'Value') || get(findobj(obj.tbuts,'tag','play'),'Value')
                    button_state = get(src,'Value');
                    if button_state == get(src,'Max')
                        offbutton = findobj(obj.tbuts,'tag','set_negative_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_high_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_negative_low_voltage');
                        set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_low_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_mid_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_midtwo_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_high_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',0);
                        offbutton = findobj(obj.tbuts,'tag','set_positive_higher_voltage');
                        set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                        set(offbutton,'Value',1);
                        % we are in setVoltage mode
                        obj.setPositiveHigherVoltage(src);
                    else
                        % we are resetted
                        obj.resetVoltage(src);
                    end
                end
            elseif strcmp(get(src,'tag'),'set_negative_zap_voltage')
                if get(findobj(obj.tbuts,'tag','record'),'Value') || get(findobj(obj.tbuts,'tag','play'),'Value')
                    %button_state = get(src,'Value');
                    %if button_state == get(src,'Max')
                    offbutton = findobj(obj.tbuts,'tag','set_negative_higher_voltage');
                    set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_negative_high_voltage');
                    set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_negative_midtwo_voltage');
                    set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_negative_mid_voltage');
                    set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_negative_low_voltage');
                    set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_positive_low_voltage');
                    set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_positive_mid_voltage');
                    set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_positive_midtwo_voltage');
                    set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_positive_high_voltage');
                    set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_positive_higher_voltage');
                    set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                    set(offbutton,'Value',0);
                    % we are in setVoltage mode
                    obj.setNegativeZapVoltage(src);
                    %else
                    % we are resetted
                    obj.resetVoltage(src);
                    %end
                end
            elseif strcmp(get(src,'tag'),'set_positive_zap_voltage')
                if get(findobj(obj.tbuts,'tag','record'),'Value') || get(findobj(obj.tbuts,'tag','play'),'Value')
                    %button_state = get(src,'Value');
                    %if button_state == get(src,'Max')
                    offbutton = findobj(obj.tbuts,'tag','set_negative_higher_voltage');
                    set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_negative_high_voltage');
                    set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_negative_midtwo_voltage');
                    set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_negative_mid_voltage');
                    set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_negative_low_voltage');
                    set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_positive_low_voltage');
                    set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_positive_mid_voltage');
                    set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_positive_midtwo_voltage');
                    set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_positive_high_voltage');
                    set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                    set(offbutton,'Value',0);
                    offbutton = findobj(obj.tbuts,'tag','set_positive_higher_voltage');
                    set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                    set(offbutton,'Value',0);
                    % we are in setVoltage mode
                    obj.setPositiveZapVoltage(src);
                    %else
                    % we are resetted
                    obj.resetVoltage(src);
                    %end
                end
            elseif strcmp(get(src,'tag'),'set_gauge_value')
                obj.setGaugeValue(src);

            elseif strcmp(get(src,'tag'),'noise')
                button_state = get(src,'Value');
                if button_state == get(src,'Max')
                    % we are in noise display mode
                    obj.startNoiseDisplay(src);
                else
                    % we are stopped
                    obj.stopNoiseDisplay(src);
                end
            elseif strcmp(get(src,'tag'),'iv_sweep_up')
                button_state = get(src,'Value');
                if button_state == get(src,'Max')
                    % we are in noise display mode
                    obj.startIV(src);
                else
                    % we are stopped
                    obj.stopIV(src);
                end
            elseif strcmp(get(src,'tag'),'iv_sweep_down')
                button_state = get(src,'Value');
                if button_state == get(src,'Max')
                    % we are in noise display mode
                    obj.startIV(src);
                else
                    % we are stopped
                    obj.stopIV(src);
                end
            elseif strcmp(get(src,'tag'),'iv_sweep_bi')
                button_state = get(src,'Value');
                if button_state == get(src,'Max')
                    % we are in noise display mode
                    obj.startIV(src);
                else
                    % we are stopped
                    obj.stopIV(src);
                end
            elseif strcmp(get(src,'tag'),'sealtest_play')
                button_state = get(src,'Value');
                if button_state == get(src,'Max')
                    recbutton = findobj(obj.tbuts,'tag','sealtest_record');
                    obj.stopSealTestRecording(recbutton);
                    set(recbutton,'Value',0);
                    playbutton = findobj(obj.tbuts,'tag','sealtest_play');
                    % we are in seal test mode
                    obj.startSealTestPlay(src);
                    set(playbutton,'Value',1);
                else
                    % we are stopped
                    playbutton = findobj(obj.tbuts,'tag','sealtest_play');
                    obj.stopSealTestPlay(src);
                    set(playbutton,'Value',0);
                end
            elseif strcmp(get(src,'tag'),'sealtest_record')
                button_state = get(src,'Value');
                if button_state == get(src,'Max')
                    playbutton = findobj(obj.tbuts,'tag','sealtest_play');
                    obj.stopSealTestPlay(playbutton);
                    set(playbutton,'Value',0);
                    recbutton = findobj(obj.tbuts,'tag','sealtest_record');
                    % we are in seal test mode
                    obj.startSealTestRecording(src);
                    set(recbutton,'Value',1);
                else
                    % we are stopped
                    recbutton = findobj(obj.tbuts,'tag','sealtest_record');
                    obj.stopSealTestRecording(src);
                    savefig(obj.fig,[obj.file.folder obj.file.prefix '_' sprintf('%04d',obj.file.num) '_seal.fig']);%liyi 2021-08-01
                    set(recbutton,'Value',0);
                end
            elseif strcmp(get(src,'tag'),'freqresp_play')
                button_state = get(src,'Value');
                if button_state == get(src,'Max')
                    recbutton = findobj(obj.tbuts,'tag','freqresp_record');
                    %obj.stopFreqRespRecording(recbutton);
                    set(recbutton,'Value',0);
                    playbutton = findobj(obj.tbuts,'tag','freqresp_play');
                    % we are in seal test mode
                    obj.startFreqRespPlay(src);
                    set(playbutton,'Value',1);
                else
                    % we are stopped
                    playbutton = findobj(obj.tbuts,'tag','freqresp_play');
                    obj.stopFreqRespPlay(src);
                    set(playbutton,'Value',0);
                end
            elseif strcmp(get(src,'tag'),'freqresp_record')
                button_state = get(src,'Value');
                if button_state == get(src,'Max')
                    playbutton = findobj(obj.tbuts,'tag','freqresp_play');
                    obj.stopFreqRespPlay(playbutton);
                    set(playbutton,'Value',0);
                    recbutton = findobj(obj.tbuts,'tag','freqresp_record');
                    % we are in seal test mode
                    obj.startFreqRespRecording(src);
                    set(recbutton,'Value',1);
                else
                    % we are stopped
                    recbutton = findobj(obj.tbuts,'tag','freqresp_record');
                    obj.stopFreqRespRecording(src);
                    savefig(obj.fig,[obj.file.folder obj.file.prefix '_' sprintf('%04d',obj.file.num) '_seal.fig']);%liyi 2021-08-01
                    set(recbutton,'Value',0);
                end
            end
        end

        function play(obj, button)
            % begins displaying data live from the DAQ
            try
                set(button,'CData',imread('Pause.png'));
                set(button,'String','');
                set(button,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_negative_zap_voltage');
                set(offbutton,'CData',imread('Voltage_NEGZAP_On.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_negative_higher_voltage');
                set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_negative_high_voltage');
                set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_negative_midtwo_voltage');
                set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_negative_mid_voltage');
                set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_negative_low_voltage');
                set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_positive_low_voltage');
                set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_positive_mid_voltage');
                set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_positive_midtwo_voltage');
                set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_positive_high_voltage');
                set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_positive_higher_voltage');
                set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_positive_zap_voltage');
                set(offbutton,'CData',imread('Voltage_POSZAP_On.png'));
                set(offbutton,'Value',0);
            catch ex
                set(button,'String','Pause');
            end
            try
                pause(0.25); % liyi 2021-08-01
                obj.fig_cache.clear_fig;

                queueOutputData(obj.DAQ.ao1,[0*ones(1,5)]'*obj.outputAlpha);%[zeros(1,5)]'*obj.outputAlpha); %liyi 2023-05-30
                obj.DAQ.ao1.startBackground; %liyi 2023-05-30
                obj.DAQ.ao1.stop;
                obj.DAQ.ao1.IsContinuous = false;

                queueOutputData(obj.DAQ.ao0,[zeros(1,5)]'*obj.outputAlpha); %liyi 2021-03-12
                obj.DAQ.ao0.startBackground; %liyi 2021-03-12
                obj.DAQ.ao0.stop;
                obj.DAQ.ao0.IsContinuous = false;

                % add listener for data
                obj.DAQ.listeners.plot = addlistener(obj.DAQ.s, 'DataAvailable', ...
                    @(~,event) obj.showData(event));

                % start DAQ session
                obj.DAQ.s.startBackground;

            catch ex
                display('Error obtaining data from DAQ.')
            end
        end

        function stopPlay(obj, button)
            % Data viewing off
            try
                set(button,'CData',imread('Play.png'));
                set(button,'String','');
                set(button,'Value',0);
            catch ex
                set(button,'String','Play');
            end
            try
                obj.DAQ.s.stop;
                delete(obj.DAQ.listeners.plot);
                queueOutputData(obj.DAQ.ao0,[zeros(1,5)]'*obj.outputAlpha); %liyi 2021-03-12
                obj.DAQ.ao0.startBackground; %liyi 2021-03-12
                obj.DAQ.ao0.stop;
                obj.DAQ.ao0.IsContinuous = false;
                queueOutputData(obj.DAQ.ao1,[0*ones(1,5)]'*obj.outputAlpha);%[zeros(1,5)]'*obj.outputAlpha); %liyi 2023-05-30
                obj.DAQ.ao1.startBackground; %liyi 2023-05-30
                obj.DAQ.ao1.stop;
                obj.DAQ.ao1.IsContinuous = false;
            catch ex

            end
        end

        function record(obj, button)
            % begins displaying data live from the DAQ
            % and records it to a file in the designated save location
            try
                set(button,'CData',imread('Recording.png'));
                set(button,'String','');
                set(button,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_negative_zap_voltage');
                set(offbutton,'CData',imread('Voltage_NEGZAP_On.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_positive_zap_voltage');
                set(offbutton,'CData',imread('Voltage_POSZAP_On.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_negative_higher_voltage');
                set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_negative_high_voltage');
                set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_negative_midtwo_voltage');
                set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_negative_mid_voltage');
                set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_negative_low_voltage');
                set(offbutton,'CData',imread('Voltage_NV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_positive_low_voltage');
                set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_positive_mid_voltage');
                set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_positive_midtwo_voltage');
                set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                set(offbutton,'Value',0);
                offbutton = findobj(obj.tbuts,'tag','set_positive_high_voltage');
                set(offbutton,'CData',imread('Voltage_PV_Off.png'));
                set(offbutton,'Value',0);
            catch ex
                set(button,'String','Recording!');
            end
            try
                pause(0.5); % liyi 2021-08-01
                obj.fig_cache.clear_fig;

                queueOutputData(obj.DAQ.ao0,[zeros(1,5)]'*obj.outputAlpha); %liyi 2021-03-12
                obj.DAQ.ao0.startBackground; %liyi 2021-03-12

                % make sure not to write over an existing file
                c = clock; % update date prefix
                prefix = [num2str(c(1)) '_' sprintf('%02d',c(2)) '_' sprintf('%02d',c(3))];
                % if there is a file with the same name in this folder
                num = obj.file.num;
                while ~isempty(ls([obj.file.folder prefix '_' sprintf('%04d',num) '*']))
                    num = num + 1; % increment the file number for the next file
                end
                % update the handle info
                obj.file.name = [obj.file.folder prefix '_' sprintf('%04d',num) obj.file.suffix];
                obj.file.prefix = prefix;
                obj.file.num = num;
                % get new file ready and open
                fid = fopen(obj.file.name,'w');
                obj.writeFileHeader(fid);
                % add listener for data
                obj.DAQ.listeners.logAndPlot = addlistener(obj.DAQ.s, 'DataAvailable', ...
                    @(~,event) obj.showDataAndRecord(event, fid));
                % start DAQ session
                obj.DAQ.s.startBackground;

                obj.file.fid = fid;
                display('Saving data to');
                display(obj.file.name);
            catch ex
                display('Error Recording!');
            end
        end

        function stopRecord(obj, button)
            % Data viewing and recording off
            try
                set(button,'CData',imread('Record.png'));
                set(button,'String','');
                set(button,'Value',0);
            catch ex
                %set(button,'String','stopRecord');
            end
            try
                fclose(obj.file.fid);
                obj.DAQ.s.stop;
                delete(obj.DAQ.listeners.logAndPlot);
                queueOutputData(obj.DAQ.ao0,[zeros(1,5)]'*obj.outputAlpha); %liyi 2021-03-12
                obj.DAQ.ao0.startBackground; %liyi 2021-03-12
            catch ex
                %set(button,'String','stopRecord');
            end
        end

        function setNegativeZapVoltage(obj, button)
            % begins displaying data live from the DAQ
            try
                set(button,'CData',imread('Voltage_NEGZAP_On.png'));
                set(button,'String','');
            catch ex
                set(button,'String','setVoltage');
            end
            try
                queueOutputData(obj.DAQ.ao0,[-1.3*ones(1,5) 0.0*ones(1,5)]'*obj.outputAlpha); %liyi 2021-03-12
                obj.DAQ.ao0.startBackground; %liyi 2021-03-12
            catch ex
                display('Error setting a voltage to DAQ.')
            end
        end
        function setPositiveZapVoltage(obj, button)
            % begins displaying data live from the DAQ
            try
                set(button,'CData',imread('Voltage_POSZAP_On.png'));
                set(button,'String','');
            catch ex
                set(button,'String','setVoltage');
            end
            try
                queueOutputData(obj.DAQ.ao0,[1.3*ones(1,5) 0.0*ones(1,5)]'*obj.outputAlpha); %liyi 2021-03-12
                obj.DAQ.ao0.startBackground; %liyi 2021-03-12
            catch ex
                display('Error setting a voltage to DAQ.')
            end
        end
        function setNegativeHigherVoltage(obj, button)
            % begins displaying data live from the DAQ
            try
                set(button,'CData',imread('Voltage_NV_On.png'));
                set(button,'String','');
            catch ex
                set(button,'String','setVoltage');
            end
            try
                queueOutputData(obj.DAQ.ao0,[-0.40*ones(1,5)]'*obj.outputAlpha); %liyi 2021-03-12
                obj.DAQ.ao0.startBackground; %liyi 2021-03-12
            catch ex
                display('Error setting a voltage to DAQ.')
            end
        end
        function setNegativeHighVoltage(obj, button)
            % begins displaying data live from the DAQ
            try
                set(button,'CData',imread('Voltage_NV_On.png'));
                set(button,'String','');
            catch ex
                set(button,'String','setVoltage');
            end
            try
                queueOutputData(obj.DAQ.ao0,[-0.20*ones(1,5)]'*obj.outputAlpha); %mcy_TIA_00_220816
                obj.DAQ.ao0.startBackground; %liyi 2021-03-12
            catch ex
                display('Error setting a voltage to DAQ.')
            end
        end
        function setNegativeMidtwoVoltage(obj, button)
            % begins displaying data live from the DAQ
            try
                set(button,'CData',imread('Voltage_NV_On.png'));
                set(button,'String','');
            catch ex
                set(button,'String','setVoltage');
            end
            try
                queueOutputData(obj.DAQ.ao0,[-0.15*ones(1,5)]'*obj.outputAlpha); %mcy_TIA_00_220816
                obj.DAQ.ao0.startBackground; %liyi 2021-03-12
            catch ex
                display('Error setting a voltage to DAQ.')
            end
        end
        function setNegativeMidVoltage(obj, button)
            % begins displaying data live from the DAQ
            try
                set(button,'CData',imread('Voltage_NV_On.png'));
                set(button,'String','');
            catch ex
                set(button,'String','setVoltage');
            end
            try
                queueOutputData(obj.DAQ.ao0,[-0.10*ones(1,5)]'*obj.outputAlpha); %mcy_TIA_00_220816
                obj.DAQ.ao0.startBackground; %liyi 2021-03-12
            catch ex
                display('Error setting a voltage to DAQ.')
            end
        end
        function setNegativeLowVoltage(obj, button)
            % begins displaying data live from the DAQ
            try
                set(button,'CData',imread('Voltage_NV_On.png'));
                set(button,'String','');
            catch ex
                set(button,'String','setVoltage');
            end
            try
                queueOutputData(obj.DAQ.ao0,[-0.05*ones(1,5)]'*obj.outputAlpha); %mcy_TIA_00_220816
                obj.DAQ.ao0.startBackground; %liyi 2021-03-12
            catch ex
                display('Error setting a voltage to DAQ.');
            end
        end
        function setPositiveLowVoltage(obj, button)
            % begins displaying data live from the DAQ
            try
                set(button,'CData',imread('Voltage_PV_On.png'));
                set(button,'String','');
            catch ex
                set(button,'String','setVoltage');
            end
            try
                queueOutputData(obj.DAQ.ao0,[0.05*ones(1,5)]'*obj.outputAlpha); %mcy_TIA_00_220816
                obj.DAQ.ao0.startBackground; %liyi 2021-03-12
            catch ex
                display('Error setting a voltage to DAQ.');
            end
        end
        function setPositiveMidVoltage(obj, button)
            % begins displaying data live from the DAQ
            try
                set(button,'CData',imread('Voltage_PV_On.png'));
                set(button,'String','');
            catch ex
                set(button,'String','setVoltage');
            end
            try
                queueOutputData(obj.DAQ.ao0,[0.10*ones(1,5)]'*obj.outputAlpha); %mcy_TIA_00_220816
                obj.DAQ.ao0.startBackground;
            catch ex
                display('Error setting a voltage to DAQ.')
            end
        end
        function setPositiveMidtwoVoltage(obj, button)
            % begins displaying data live from the DAQ
            try
                set(button,'CData',imread('Voltage_PV_On.png'));
                set(button,'String','');
            catch ex
                set(button,'String','setVoltage');
            end
            try
                queueOutputData(obj.DAQ.ao0,[0.15*ones(1,5)]'*obj.outputAlpha); %mcy_TIA_00_220816
                obj.DAQ.ao0.startBackground; %liyi 2021-03-12
            catch ex
                display('Error setting a voltage to DAQ.')
            end
        end
        function setPositiveHighVoltage(obj, button)
            % begins displaying data live from the DAQ
            try
                set(button,'CData',imread('Voltage_PV_On.png'));
                set(button,'String','');
            catch ex
                set(button,'String','setVoltage');
            end
            try
                queueOutputData(obj.DAQ.ao0,[0.20*ones(1,5)]'*obj.outputAlpha); %mcy_TIA_00_220816
                obj.DAQ.ao0.startBackground; %mcy_TIA_00 2022-08-07
            catch ex
                display('Error setting a voltage to DAQ.')
            end
        end
        function setPositiveHigherVoltage(obj, button)
            % begins displaying data live from the DAQ
            try
                set(button,'CData',imread('Voltage_PV_On.png'));
                set(button,'String','');
            catch ex
                set(button,'String','setVoltage');
            end
            try
                queueOutputData(obj.DAQ.ao0,[0.40*ones(1,5)]'*obj.outputAlpha); %mcy_TIA_00_220816
                obj.DAQ.ao0.startBackground; %mcy_TIA_00 2022-08-07
            catch ex
                display('Error setting a voltage to DAQ.')
            end
        end
        
        function resetVoltage(obj, button)
            % Data viewing off
            try
                if strcmp(get(button,'tag'),'set_positive_higher_voltage')
                    set(button,'CData',imread('Voltage_PV_Off.png'));
                end
                if strcmp(get(button,'tag'),'set_positive_high_voltage')
                    set(button,'CData',imread('Voltage_PV_Off.png'));
                end
                if strcmp(get(button,'tag'),'set_positive_midtwo_voltage')
                    set(button,'CData',imread('Voltage_PV_Off.png'));
                end
                if strcmp(get(button,'tag'),'set_positive_mid_voltage')
                    set(button,'CData',imread('Voltage_PV_Off.png'));
                end
                if strcmp(get(button,'tag'),'set_positive_low_voltage')
                    set(button,'CData',imread('Voltage_PV_Off.png'));
                end
                if strcmp(get(button,'tag'),'set_negative_low_voltage')
                    set(button,'CData',imread('Voltage_NV_Off.png'));
                end
                if strcmp(get(button,'tag'),'set_negative_mid_voltage')
                    set(button,'CData',imread('Voltage_NV_Off.png'));
                end
                if strcmp(get(button,'tag'),'set_negative_midtwo_voltage')
                    set(button,'CData',imread('Voltage_NV_Off.png'));
                end
                if strcmp(get(button,'tag'),'set_negative_high_voltage')
                    set(button,'CData',imread('Voltage_NV_Off.png'));
                end
                if strcmp(get(button,'tag'),'set_negative_higher_voltage')
                    set(button,'CData',imread('Voltage_NV_Off.png'));
                end
                set(button,'String','');
            catch ex
                set(button,'String','resetVoltage');
            end
            try
                queueOutputData(obj.DAQ.ao0,[zeros(1,5)]'*obj.outputAlpha); %liyi 2021-03-12
                obj.DAQ.ao0.startBackground; %liyi 2021-03-12
                obj.DAQ.ao0.stop;
                obj.DAQ.ao0.IsContinuous = false;
                queueOutputData(obj.DAQ.ao1,[0*ones(1,5)]'*obj.outputAlpha);%[zeros(1,5)]'*obj.outputAlpha); %liyi 2023-05-30
                obj.DAQ.ao1.startBackground; %liyi 2021-03-12
                obj.DAQ.ao1.stop;
                obj.DAQ.ao1.IsContinuous = false;
            catch ex

            end
        end

        % Create ValueChangedFcn callback

        function setGaugeValue(obj, button)

            try
                %queueOutputData(obj.DAQ.ao1,[0*ones(1,5)]'*obj.outputAlpha);%[zeros(1,5)]'*obj.outputAlpha); %liyi 2023-05-30
                %obj.DAQ.ao1.startBackground; %liyi 2021-03-12
                %obj.DAQ.ao1.stop;
                %obj.DAQ.ao1.IsContinuous = false;
            catch ex

            end
        end

        function processedData = processDifferentialData(obj, rawData)
            % Process data to compute differential signals where configured
            % rawData is [timestamps, channel0, channel1, ..., channel7]
            processedData = rawData; % Start with original data
            
            % Initialize save flags - by default save all enabled channels
            obj.saveChannelFlags = obj.channelEnabled;
            
            % Track which channels have been processed as differential
            processedChannels = false(1, 8);
            
            for i = 1:8
                if processedChannels(i)
                    continue;
                end
                
                userData = get(obj.chbuts(i), 'UserData');
                if isempty(userData) || ~isfield(userData, 'isDifferential')
                    continue;
                end
                
                if userData.isDifferential && userData.pairChannel > 0
                    lowIdx = i;
                    highIdx = userData.pairChannel;
                    
                    % Calculate differential signal: low - high
                    % rawData column indices: 1=time, 2=ch0, 3=ch1, etc.
                    lowDataCol = lowIdx + 1;
                    highDataCol = highIdx + 1;
                    
                    % Replace low channel with differential signal
                    processedData(:, lowDataCol) = rawData(:, lowDataCol) - rawData(:, highDataCol);
                    % Zero out high channel (will not be saved)
                    processedData(:, highDataCol) = 0;
                    
                    % Update save flags: save low channel, don't save high channel
                    obj.saveChannelFlags(lowIdx) = true;   % Save differential result
                    obj.saveChannelFlags(highIdx) = false; % Don't save high channel
                    
                    processedChannels(lowIdx) = true;
                    processedChannels(highIdx) = true;
                    
                    fprintf('Differential signal computed: CH%d - CH%d -> CH%d\n', lowIdx-1, highIdx-1, lowIdx-1);
                end
            end
        end
        
        function showData(obj, evt)
            % Process differential mode before display
            dataWithTime = [evt.TimeStamps, evt.Data];
            processedData = obj.processDifferentialData(dataWithTime);
            obj.fig_cache.update_cache(processedData);
            obj.fig_cache.draw_fig_now();
            obj.updateSplitCaches(processedData);
        end

        function showDataAndRecord(obj, evt, fid)
            % Process differential mode before recording and display
            dataWithTime = [evt.TimeStamps, evt.Data];
            processedData = obj.processDifferentialData(dataWithTime);
            
            % Scale and write to file (only enabled channels)
            % processedData columns: [time, ch0, ch1, ..., ch7]
            data = (repmat(obj.alpha,size(processedData,1),1) .* processedData(:,2:end))';
            obj.writeFileData(fid, data);
            
            % Update display
            obj.fig_cache.update_cache(processedData);
            obj.fig_cache.draw_fig_now();
            obj.updateSplitCaches(processedData);
        end

        function startNoiseDisplay(obj, button)
            % Sample chunks of data, calculate noise, and display it
            try
                set(button,'CData',imread('Pause.png'));
                set(button,'Value',1);
                set(button,'String','');
            catch ex
                set(button,'String','Pause');
            end
            try
                % add listener for data
                obj.DAQ.s.NotifyWhenDataAvailableExceeds = 2^16;
                obj.DAQ.listeners.noise = addlistener(obj.DAQ.s, 'DataAvailable', ...
                    @(~,event) obj.plotNoise(event));
                % start DAQ session
                obj.DAQ.s.startBackground;

            catch ex

            end
        end

        function stopNoiseDisplay(obj, button)
            try
                set(button,'CData',imread('Play.png'));
                set(button,'String','');
            catch ex
                set(button,'String','Play');
            end
            try
                obj.DAQ.s.stop;
                obj.DAQ.s.IsNotifyWhenDataAvailableExceedsAuto = true; % set back to auto
                delete(obj.DAQ.listeners.noise);
                savefig(obj.fig,[obj.file.folder obj.file.prefix '_' sprintf('%04d',obj.file.num) '_noise.fig']);%liyi 2021-08-01
            catch ex

            end
        end

        function plotNoise(obj, evt)
            % Calculate the noise power spectral density, and plot it
            % calculation
            fftsize = size(evt.Data,1);%min(size(evt.Data,1),2^16);
            f = obj.sampling*(0:fftsize/2)/fftsize; % frequency range

            % plot only enabled channels
            cla(obj.axes);
            colorOrder = get(obj.axes, 'ColorOrder');
            
            % Track which channels have been processed as differential
            processedChannels = false(1, 8);
            
            for i=1:size(obj.alpha,2)
                % Skip if channel is not enabled
                if ~obj.channelEnabled(i)
                    continue;
                end
                
                % Skip if already processed as part of a differential pair
                if processedChannels(i)
                    continue;
                end
                
                userData = get(obj.chbuts(i), 'UserData');
                
                if ~isempty(userData) && isfield(userData, 'isDifferential') && userData.isDifferential && userData.pairChannel > 0
                    % This is a differential pair
                    lowIdx = i;
                    highIdx = userData.pairChannel;
                    
                    % Calculate differential signal: low - high
                    diffData = evt.Data(:, lowIdx) - evt.Data(:, highIdx);
                    
                    % Plot differential noise with special label
                    dfft1 = 1/obj.sampling*abs(fft(obj.alpha(lowIdx)/1000*diffData)).^2/fftsize;
                    dfft1 = dfft1(1:fftsize/2+1);
                    dfft1 = 2*dfft1;
                    
                    plot(obj.axes, f', dfft1*1e6, 'Color', colorOrder(lowIdx, :), 'DisplayName', sprintf('CH%d-%d', lowIdx-1, highIdx-1));
                    
                    % Mark both channels as processed
                    processedChannels(lowIdx) = true;
                    processedChannels(highIdx) = true;
                else
                    % Single-ended channel
                    dfft1 = 1/obj.sampling*abs(fft(obj.alpha(i)/1000*evt.Data(:,i))).^2/fftsize; % fft!
                    dfft1 = dfft1(1:fftsize/2+1);
                    dfft1 = 2*dfft1;
                    
                    plot(obj.axes, f', dfft1*1e6, 'Color', colorOrder(i, :), 'DisplayName', sprintf('CH%d', i-1));
                    processedChannels(i) = true;
                end
                
                if sum(processedChannels(obj.channelEnabled)) == 1
                    hold on
                end
            end
            
            % Add legend
            legend(obj.axes, 'show');
            drawnow;
        end

        function startIV(obj, button)
            % Current versus voltage curve
            % Apply -200:10:200mV for 200ms each (adjustable)
            % Plot data
            % Take the average current(voltage) and plot that as well
            try
                set(button,'CData',imread('Pause.png'));
                set(button,'Value',1);
                set(button,'String','');
            catch ex
                set(button,'String','Stop');
            end
            try
                % get the desired sweep from user
                obj.DAQ.ao0.IsContinuous = true;
                holdtime = 0.1; % in seconds, hold at 0mV before and after
                prompt = {'Starting voltage (mV):', ...
                    'Step by (mV):', ...
                    'Ending voltage (mV)', ...
                    'Duration of each voltage (ms):'};
                dlg_title = 'IV curve setup';
                defaultans = {'200','-20','-200','600'};
                answer = inputdlg(prompt,dlg_title,1,defaultans);
                % defAns????????????prompt??????????????
                if isempty(answer) % user pressed 'cancel'
                    obj.stopIV(button);
                    set(button,'Value',0);
                else % user entered an IV program
                    if strcmp(get(button,'tag'),'iv_sweep_down')
                        answer = str2double(answer);
                        %V = linspace(answer(1),answer(3),(answer(3)-answer(1))/answer(2)+1)*0.001; % in Volts
                        A = linspace(0,answer(1),-answer(1)/answer(2)+1)*0.001; % in Volts % liyi 2021/07/30
                        B = -A;
                        B(1) = [];
                        V = [A,B]';
                        %V = V(:);
                        t = answer(4)*0.001; % time in seconds
                    end
                    if strcmp(get(button,'tag'),'iv_sweep_up')
                        answer = str2double(answer);
                        %V = linspace(answer(1),answer(3),(answer(3)-answer(1))/answer(2)+1)*0.001; % in Volts % liyi 2021/07/30
                        %V = -V;
                        A = linspace(0,answer(1),-answer(1)/answer(2)+1)*0.001; % in Volts % liyi 2021/07/30
                        B = -A;
                        A(1) = [];
                        V = [B,A]';
                        %V = V(:);
                        t = answer(4)*0.001; % time in seconds
                    end
                    if strcmp(get(button,'tag'),'iv_sweep_bi')
                        answer = str2double(answer);
                        A = linspace(0,answer(1),-answer(1)/answer(2)+1)*0.001; % in Volts % liyi 2021/07/30
                        B = -A;
                        V = [A;B];
                        V = V(:);
                        V(1) = [];
                        t = answer(4)*0.001; % time in seconds
                    end

                    % make sure not to write over an existing file
                    c = clock; % update date prefix
                    prefix = [num2str(c(1)) '_' sprintf('%02d',c(2)) '_' sprintf('%02d',c(3))];
                    % if there is a file with the same name in this folder
                    num = obj.file.num;
                    while ~isempty(ls([obj.file.folder prefix '_' sprintf('%04d',num) '*']))
                        num = num + 1; % increment the file number for the next file
                    end
                    % update the handle info
                    obj.file.name = [obj.file.folder prefix '_' sprintf('%04d',num) '_IV' obj.file.suffix];
                    obj.file.prefix = prefix;
                    obj.file.num = num;
                    % get new file ready and open
                    obj.file.fid = fopen(obj.file.name,'w');
                    obj.writeFileHeader(obj.file.fid);
                    display('Saving data to');
                    display(obj.file.name);

                    % program the output voltages
                    for i = 1:numel(V)-1
                        queueOutputData(obj.DAQ.ao0,[zeros(1,obj.outputFreq*holdtime), ...
                            V(i)*ones(1,obj.outputFreq*t), ...
                            zeros(1,obj.outputFreq*holdtime)]'*obj.outputAlpha);
                    end
                    % there must be a bit extra to trigger last sweep
                    queueOutputData(obj.DAQ.ao0,[zeros(1,obj.outputFreq*holdtime), ...
                        V(end)*ones(1,obj.outputFreq*t), ...
                        zeros(1,obj.outputFreq*holdtime+5)]'*obj.outputAlpha);

                    global special_counter
                    special_counter = 0;
                    % how to plot
                    nchans = numel(obj.channels);
                    obj.DAQ.s.NotifyWhenDataAvailableExceeds = nchans*obj.sampling*holdtime + obj.sampling*t; % points in each sweep
                    obj.DAQ.listeners.IVsweep = addlistener(obj.DAQ.s, 'DataAvailable', ...
                        @(~,event) obj.plotIV(event, holdtime, 2*holdtime+t, V, obj.file.fid));
                    cla(obj.axes(1));
                    obj.axes(1).XLim = [0, 2*holdtime+t];
                    %obj.axes(1).YLim = [-2*max(I), 2*max(I)];
                    cla(obj.axes(2));
                    obj.axes(2).XLim = [min(V)*1000-10, max(V)*1000+10];
                    %obj.axes(2).YLim = [-2*max(I), 2*max(I)];


                    % start DAQ session
                    obj.DAQ.ao0.startBackground;
                    %obj.DAQ.ao1.startBackground;
                    obj.DAQ.s.startBackground;

                    % wait until done and then kill it
                    pause(numel(V)*(2*holdtime+t+0.1)); % THIS IS BAD... NEED EVENT
                    set(button,'Value',0);
                    obj.stopIV(button);
                    savefig(obj.fig,[obj.file.folder obj.file.prefix '_' sprintf('%04d',obj.file.num) '_IV.fig']);%liyi 2021-08-01
                end
            catch ex

            end
        end

        function stopIV(obj, button)
            % stop the IV curve stuff
            try
                if strcmp(get(button,'tag'),'iv_sweep_up')
                    set(button,'CData',imread('Sweep_Up.png'));
                end
                if strcmp(get(button,'tag'),'iv_sweep_down')
                    set(button,'CData',imread('Sweep_Down.png'));
                end
                if strcmp(get(button,'tag'),'iv_sweep_bi')
                    set(button,'CData',imread('Sweep_Bi.png'));
                end
                set(button,'String','');
            catch ex
                set(button,'String','Start');
            end
            try
                fclose(obj.file.fid);
                obj.DAQ.s.stop;
                obj.DAQ.ao0.stop;
                obj.DAQ.ao0.IsContinuous = false;
                delete(obj.DAQ.listeners.IVsweep);
            catch ex

            end
        end

        function plotIV(obj, evt, holdtime, sweeptime, V, fid)
            % when a full sweep of data comes in, this event fires
            % plots the sweep on axis 1
            % plots a mean data point on axis 2

            % save the data to a file
            data = (repmat(obj.alpha,size(evt.Data,1),1) .* evt.Data)';
            % data = data(1:2,:); % only use the first and second input channel
            obj.writeFileData(fid, data);

            temp = filt_lpb([(1:size(data,2))'/obj.sampling, data'],4,5000)';% liyi 2022-08-17 1000Hz
            %obj.writeFileData(fid, temp(2:3,:));% liyi 2022-08-17

            if size(obj.alpha,2)==2
                % plot the data
                %plot(obj.axes(1), linspace(0,sweeptime,size(evt.Data,1)), evt.Data(:,1)*obj.alpha(1)/1000, 'Color', 'k');
                plot(obj.axes(1), linspace(0,sweeptime,size(evt.Data,1)), temp(2,:)', 'Color', 'k');% liyi 2022-08-17
                measuretime = sweeptime-2*holdtime;
                inds = round((holdtime+measuretime/5)*obj.sampling:(holdtime+4*measuretime/5)*obj.sampling);
                voltage = mean(temp(3,inds)'); % mV
                current = mean(temp(2,inds)');
                voltage_rms = std(temp(3,inds)'); % mV
                current_rms = std(temp(2,inds)');
                %plot(obj.axes(2), voltage, current, 'ok');
                errorbar(obj.axes(2), voltage, current, current_rms, current_rms, 'ok'); % liyi 2021-08-01
                %savefig(obj.fig,[obj.file.folder obj.file.prefix '_' sprintf('%04d',obj.file.num) '_IV.fig']);%liyi 2021-08-01
            else
                plot(obj.axes(1), linspace(0,sweeptime,size(evt.Data,1)), temp(2:end,:)');% liyi 2022-08-17
                measuretime = sweeptime-2*holdtime;
                inds = round((holdtime+measuretime/5)*obj.sampling:(holdtime+4*measuretime/5)*obj.sampling);
                global special_counter
                special_counter = special_counter+1;
                voltage = V(special_counter).*ones(size(obj.alpha,2),1)*1000;
                current = mean(temp(2:end,inds)');
                voltage_rms = zeros(size(obj.alpha,2),1);
                current_rms = std(temp(2:end,inds)');
                for i = 1:size(obj.alpha,2)
                    errorbar(obj.axes(2), voltage(i), current(i), current_rms(i), current_rms(i), 'o'); % liyi 2021-08-01
                end
            end
        end

        function startSealTestPlay(obj, button)
            % Membrane "seal test" display
            % Apply a 5mV square wave
            % Measure current
            % Display as would an oscilloscope on a trigger
            obj.DAQ.ao0.IsContinuous = true;
            v = 0.4; % 5mV
            ontime = 0.005; % time of pulse, sec
            rest = 0.005; % time of rest, sec
            onpts = round(ontime*obj.outputFreq);
            offpts = round(rest*obj.outputFreq);
            try
                set(button,'CData',imread('Pause.png'));
                set(button,'String','');
            catch ex
                set(button,'String','Stop');
            end

            try
                % program the output voltages to go on indefinitely
                %sig = [zeros(1,round(offpts/2)), ...
                %    v*ones(1,onpts), ...
                %    zeros(1,round(offpts/2))]'*obj.outputAlpha;
                sig = [zeros(1,offpts), v*ones(1,onpts)]'*obj.outputAlpha;
                queueOutputData(obj.DAQ.ao0,repmat(sig,250,1));
                obj.DAQ.listeners.sealtestV = addlistener(obj.DAQ.ao0, ...
                    'DataRequired', @(~,~) queueOutputData(obj.DAQ.ao0,repmat(sig,250,1)));

                % how to plot
                obj.DAQ.s.NotifyWhenDataAvailableExceeds = obj.sampling/100*(onpts+offpts); % points in each sweep
                obj.DAQ.listeners.sealtest = addlistener(obj.DAQ.s, 'DataAvailable', ...
                    @(~,event) obj.plotSealTest(event, (onpts+offpts)/100));
                plot(obj.axes,linspace(-onpts/100,2*onpts/100,100),zeros(1,100))
                obj.axes.XLim = [0, 0.05];

                % start DAQ session
                obj.DAQ.ao0.startBackground;
                obj.DAQ.s.startBackground;
            catch ex
                display('Seal test play issue')
            end
        end

        function startSealTestRecording(obj, button)
            % Membrane "seal test" display
            % Apply a 5mV square wave
            % Measure current
            % Display as would an oscilloscope on a trigger
            obj.DAQ.ao0.IsContinuous = true;
            v = 0.05; % 5mV
            ontime = 0.005; % time of pulse, sec
            rest = 0.005; % time of rest, sec
            onpts = round(ontime*obj.outputFreq);
            offpts = round(rest*obj.outputFreq);
            try
                set(button,'CData',imread('Recording.png'));
                set(button,'String','');
            catch ex
                set(button,'String','Stop');
            end

            % update the handle info
            num = obj.file.num;
            while ~isempty(ls([obj.file.folder obj.file.prefix '_' sprintf('%04d',num) '*']))
                num = num + 1; % increment the file number for the next file
            end
            obj.file.name = [obj.file.folder obj.file.prefix '_' sprintf('%04d',num) '_seal' obj.file.suffix];
            obj.file.num = num;
            % get new file ready and open
            obj.file.fid = fopen(obj.file.name,'w');
            obj.writeFileHeader(obj.file.fid);

            try
                % program the output voltages to go on indefinitely
                %sig = [zeros(1,round(offpts/2)), ...
                %    v*ones(1,onpts), ...
                %    zeros(1,round(offpts/2))]'*obj.outputAlpha;
                sig = [zeros(1,offpts), v*ones(1,onpts)]'*obj.outputAlpha;
                queueOutputData(obj.DAQ.ao0,repmat(sig,250,1));
                obj.DAQ.listeners.sealtestV = addlistener(obj.DAQ.ao0, ...
                    'DataRequired', @(~,~) queueOutputData(obj.DAQ.ao0,repmat(sig,250,1)));

                % how to plot
                obj.DAQ.s.NotifyWhenDataAvailableExceeds = obj.sampling/100*(onpts+offpts); % points in each sweep
                obj.DAQ.listeners.sealtest = addlistener(obj.DAQ.s, 'DataAvailable', ...
                    @(~,event) obj.plotSealTestRecording(event, (onpts+offpts)/100));
                plot(obj.axes,linspace(-onpts/100,2*onpts/100,100),zeros(1,100))
                obj.axes.XLim = [0, 0.05];

                % start DAQ session
                obj.DAQ.ao0.startBackground;
                obj.DAQ.s.startBackground;
            catch ex
                display('Seal test play issue')
            end
        end

        function stopSealTestPlay(obj, button)
            % stop the seal test
            try
                set(button,'CData',imread('Play.png'));
                set(button,'String','');
                set(button,'Value',0);
            catch ex
                set(button,'String','Start');
            end
            try
                obj.DAQ.s.stop;
                obj.DAQ.ao0.stop;
                obj.DAQ.ao0.IsContinuous = false;
                delete(obj.DAQ.listeners.sealtest);
                delete(obj.DAQ.listeners.sealtestV);
            catch ex

            end
        end

        function stopSealTestRecording(obj, button)
            % stop the seal test
            try
                set(button,'CData',imread('Record.png'));
                set(button,'String','');
                set(button,'Value',0);
            catch ex
                set(button,'String','Start');
            end
            try
                fclose(obj.file.fid);
                obj.DAQ.s.stop;
                obj.DAQ.ao0.stop;
                %obj.DAQ.ao1.stop;
                obj.DAQ.ao0.IsContinuous = false;
                delete(obj.DAQ.listeners.sealtest);
                delete(obj.DAQ.listeners.sealtestV);
            catch ex

            end
        end

        function plotSealTest(obj, evt, duration)
            % Plot data from the seal test function
            try
                starttime = 0;
                if ~isempty(starttime)
                    x = linspace(starttime,starttime+duration,size(evt.Data,1));
                    %obj.writeFileData(obj.file.fid, (evt.Data.*obj.alpha)');
                    temp = filt_lpb([x' evt.Data.*obj.alpha],4,5000);% liyi 2022-08-17 1000Hz
                    plot(obj.axes, x', temp(:,2:end)');
                end
            catch ex
                display('Difficulty plotting seal test data')
            end
        end

        function plotSealTestRecording(obj, evt, duration)
            % Plot data from the seal test function
            try
                starttime = 0;
                if ~isempty(starttime)
                    x = linspace(starttime,starttime+duration,size(evt.Data,1));
                    obj.writeFileData(obj.file.fid, (evt.Data.*obj.alpha)');
                    temp = filt_lpb([x' evt.Data.*obj.alpha],4,5000);% liyi 2022-08-17 1000Hz
                    plot(obj.axes, x', temp(:,2:end)');
                end
            catch ex
                display('Difficulty plotting seal test data')
            end
        end
            % Play sound
            %             try
            %                 if strcmp(obj.audio.Running,'off')
            %                     t = 0:1e-3:1*duration;
            %                     f = exp(abs(mean(evt.Data(:,1)))*1000);
            %                     if f>20
            %                         f = 100;
            %                     end
            %                     y = square(2*pi*f*t,50)+randn(size(t))/10;
            %                     obj.audio = audioplayer(y,1000);
            %                     play(obj.audio);
            %                 end
            %             catch ex
            %                 display('Could not play sound')
            %             end
        function startFreqRespPlay(obj, button)
            % Current versus voltage curve
            % Apply -200:10:200mV for 200ms each (adjustable)
            % Plot data
            % Take the average current(voltage) and plot that as well
            try
                set(button,'CData',imread('Pause.png'));
                set(button,'Value',0);
                set(button,'String','');
            catch ex
                set(button,'String','Stop');
            end
            try
                % get the desired sweep from user
                %obj.DAQ.ao0.IsContinuous = true;
                holdtime = 0.1; % in seconds, hold at 0mV before and after
                prompt = {'Starting Frequency (Hz):', ...
                    'Step by (Hz):', ...
                    'Ending Frequency (Hz)', ...
                    'Repetition of each frequency:'};
                dlg_title = 'FRA setup';
                defaultans = {'2500','-10','1','250'};
                answer = inputdlg(prompt,dlg_title,1,defaultans);
                % defAns????????????prompt??????????????
                if isempty(answer) % user pressed 'cancel'
                    obj.stopFreqResp(button);
                    set(button,'Value',0);
                else % user entered an IV program
                    obj.DAQ.s.stop;
                    obj.DAQ.ao0.stop;
                    obj.DAQ.ao0.IsContinuous = true;
                    cla(obj.axes(1));
                    cla(obj.axes(2));
                    v = 0.1; % 5mV
                    freq = [5000,2500,1250,1000,500,250,125,100,50,25];%,12.5,10,5,2.5,1.25,1];%5kHz
                    
                    % program the output frequencies
                    for i = 1:numel(freq)
                        ontime = 1/freq(i); % time of pulse, sec
                        rest = 1/freq(i); % time of rest, sec
                        onpts = round(ontime*obj.outputFreq);
                        offpts = round(rest*obj.outputFreq);
                        
                        %sig = [-v/2*ones(1,offpts), v/2*ones(1,onpts)]'*obj.outputAlpha;
                        sig = [zeros(1,offpts), v*ones(1,onpts)]'*obj.outputAlpha;
                        queueOutputData(obj.DAQ.ao0,repmat(sig,obj.sampling/size(sig,1),1));

                        %obj.DAQ.listeners.FreqResp = addlistener(obj.DAQ.ao0, ...
                        %    'DataRequired', @(~,~) queueOutputData(obj.DAQ.ao0,repmat(sig,obj.sampling/size(sig,1),1)));
                    end

                    % how to plot
                    obj.DAQ.s.NotifyWhenDataAvailableExceeds = obj.sampling/10;%*(onpts+offpts); % points in each sweep
                    obj.DAQ.listeners.freqresp = addlistener(obj.DAQ.s, 'DataAvailable', ...
                        @(~,event) obj.plotFreqRespPlay(event));
                    %plot(obj.axes,linspace(-onpts/100,2*onpts/100,100),zeros(1,100))

                    % start DAQ session
                    obj.DAQ.ao0.startBackground;
                    obj.DAQ.s.startBackground;

                    % wait until done and then kill it
                    pause(numel(freq)*5); % THIS IS BAD... NEED EVENT
                    obj.stopFreqRespPlay(button);
                    %savefig(obj.fig,[obj.file.folder obj.file.prefix '_' sprintf('%04d',obj.file.num) '_FRA.fig']);%liyi 2021-08-01
                end
            catch ex

            end
        end

        function stopFreqRespPlay(obj, button)
            % stop the IV curve stuff
            try
                set(button,'CData',imread('Play.png'));
                set(button,'Value',0);
                set(button,'String','');

                %fclose(obj.file.fid);
                obj.DAQ.s.stop;
                obj.DAQ.ao0.stop;
                obj.DAQ.ao0.IsContinuous = false;
                delete(obj.DAQ.listeners.freqresp);
            catch ex

            end
        end

        function plotFreqRespPlay(obj, evt)
            % when a full sweep of data comes in, this event fires
            % plots the sweep on axis 1
            % plots a mean data point on axis 2

            % save the data to a file
            fftsize = size(evt.Data,1);
            temp = filt_lpb([(1:fftsize)'/obj.sampling, evt.Data],4,10000)';% liyi 2022-08-17 10000Hz
            %obj.writeFileData(fid, temp(2:3,:));% liyi 2022-08-17

            if size(obj.alpha,2)==2
                % plot the data
                %plot(obj.axes(1), temp(1,:), temp(2:end,:), 'Color', 'k');% liyi 2022-08-17
                cla(obj.axes(1));
                plot(obj.axes(1), temp(1,:), temp(2:end,:));
                obj.axes(1).XLim = [0, fftsize/obj.sampling];

                
                freq = -1:0.1:9;%Hz
                freq = 10.^freq;
                omega = 2*pi*freq;
                
                R1 = 5e8;%Ohm
                Z1 = R1;
                
                R2 = 2.1e8;
                C2 = 0.05e-12;%F
                %C2= 3.3e-12;
                Z2 = 1./(1/R2+j*omega*C2);
                
                H1 = -Z2./Z1;
                
                R3 = 2e4;
                C3 = 1e-7;
                Z3 = 1./(1/R3+j*omega.*C3);

                R4 = 2e5;
                Z4 = R4;

                R5 = 5e2;
                C5 = 1e-5;
                Z5 = R5+1./(j*omega*C5);

                H2 = -Z4./Z3;

                Htotal = H1.*H2;

                Htest = 1./(Z3./Z5+1);
                
                Hpassive = (-Z2./Z1).*(1./(Z3./Z5+1));
                
                [Pout,f] = pwelch(evt.Data(:,1),[],[],fftsize,obj.sampling);
                [Pin,f] = pwelch(evt.Data(:,2),[],[],fftsize,obj.sampling);
                %cla(obj.axes(2));
                semilogx(obj.axes(2), f, 20*log10(sqrt(Pout./Pin)), 'k.');
                hold on
                semilogx(freq,20*log10(abs(Hpassive)),'r');
            else
                plot(obj.axes(1), linspace(0,sweeptime,size(evt.Data,1)), temp(2:end,:)');% liyi 2022-08-17
                measuretime = sweeptime-2*holdtime;
                inds = round((holdtime+measuretime/5)*obj.sampling:(holdtime+4*measuretime/5)*obj.sampling);
                global special_counter
                special_counter = special_counter+1;
                voltage = V(special_counter).*ones(size(obj.alpha,2),1)*1000;
                current = mean(temp(2:end,inds)');
                voltage_rms = zeros(size(obj.alpha,2),1);
                current_rms = std(temp(2:end,inds)');
                for i = 1:size(obj.alpha,2)
                    errorbar(obj.axes(2), voltage(i), current(i), current_rms(i), current_rms(i), 'o'); % liyi 2021-08-01
                end
            end
        end

        function stopAll(obj)
            % stops all DAQ sessions and deletes all listeners
            try
                obj.stopNoiseDisplay([]);
            catch ex

            end
            try
                obj.stopPlay([]);
            catch ex

            end
            try
                obj.stopRecord([]);
            catch ex

            end
            try
                obj.stopIV([]);
            catch ex

            end
            try
                obj.stopSealTest([]);
            catch ex

            end
            try
                obj.stopFreqResp([]);
            catch ex

            end
        end
        
        function toggleChannel(obj, src, channelIndex)
            % Toggle the enable/disable state of a specific channel
            button_state = get(src,'Value');
            userData = get(src, 'UserData');
            
            if button_state == get(src,'Max')
                % Channel is being ENABLED
                obj.channelEnabled(channelIndex) = true;
                obj.channelVisible(channelIndex) = true;
                set(src, 'BackgroundColor', userData.channelColor);
                display(['>>> Channel ' num2str(channelIndex-1) ' ENABLED']);
            else
                % Channel is being DISABLED
                obj.channelEnabled(channelIndex) = false;
                obj.channelVisible(channelIndex) = false;
                set(src, 'BackgroundColor', userData.disabledColor);
                display(['>>> Channel ' num2str(channelIndex-1) ' DISABLED']);
                
                % If this channel is part of a differential pair, disable the pair
                if userData.isDifferential && userData.pairChannel > 0
                    pairIdx = userData.pairChannel;
                    obj.channelEnabled(pairIdx) = false;
                    obj.channelVisible(pairIdx) = false;
                    set(obj.chbuts(pairIdx), 'Value', 0);
                    pairData = get(obj.chbuts(pairIdx), 'UserData');
                    set(obj.chbuts(pairIdx), 'BackgroundColor', pairData.disabledColor);
                    pairData.isDifferential = false;
                    pairData.pairChannel = -1;
                    set(obj.chbuts(pairIdx), 'UserData', pairData);
                    userData.isDifferential = false;
                    userData.pairChannel = -1;
                    set(src, 'UserData', userData);
                end
            end
            
            % Update channels list and recalculate sampling rate
            obj.updateChannelsAndSampling();
            
            % Update the figure cache to reflect the visibility change
            if ~isempty(obj.fig_cache) && strcmp(obj.mode, 'normal')
                obj.updateChannelDisplay();
            end
        end
        
        function updateChannelDisplay(obj)
            % Update the display based on channel visibility settings
            % Sync visibility to figure_cache
            if ~isempty(obj.fig_cache)
                obj.fig_cache.channelVisible = obj.channelVisible;
                display('Channel visibility synced to figure_cache');
                display(['Visible channels: ' num2str(find(obj.channelVisible)-1)]);
            end
            
            % Also update channel buttons for all modes (including noise plot)
            if ~isempty(obj.chbuts)
                % Get MATLAB default color order
                colorOrder = get(groot,'defaultaxescolororder');
                if size(colorOrder, 1) < 8
                    colorOrder = repmat(colorOrder, ceil(8/size(colorOrder,1)), 1);
                end
                
                % Define color scheme
                disabledColor = [0.7 0.7 0.7];   % Light gray for disabled
                
                for i = 1:8
                    channelNum = i - 1; % Channel 0-7
                    isEnabled = obj.channelEnabled(i);
                    
                    % Update button color based on enabled state
                    if isEnabled
                        userData = get(obj.chbuts(i), 'UserData');
                        if ~isempty(userData) && isfield(userData, 'isDifferential') && userData.isDifferential
                            bgColor = userData.diffColor;
                        else
                            bgColor = colorOrder(i, :);
                        end
                    else
                        bgColor = disabledColor;
                    end
                    
                    set(obj.chbuts(i), 'BackgroundColor', bgColor);
                    set(obj.chbuts(i), 'Value', isEnabled);
                    
                    % Update button label for differential mode
                    if isEnabled
                        userData = get(obj.chbuts(i), 'UserData');
                        if ~isempty(userData) && isfield(userData, 'isDifferential') && userData.isDifferential && userData.pairChannel > 0
                            lowNum = i - 1;
                            highNum = userData.pairChannel - 1;
                            set(obj.chbuts(i), 'String', sprintf('%d-%d', lowNum, highNum));
                        else
                            set(obj.chbuts(i), 'String', sprintf('CH%d', channelNum));
                        end
                    else
                        set(obj.chbuts(i), 'String', sprintf('CH%d', channelNum));
                    end
                end
            end

            % Refresh split view layout and caches when active
            if obj.splitMode
                obj.createSplitAxes();
            end
        end
        
        function updateChannelsAndSampling(obj)
            % Update sampling rate based on enabled channels
            % USB-6003 constraints: Single channel max 50kHz, Total bandwidth 100kHz
            try
                % Count effective channels (differential pairs count as 1)
                effectiveChannelCount = 0;
                processedChannels = false(1, 8);
                
                for i = 1:8
                    if obj.channelEnabled(i) && ~processedChannels(i)
                        userData = get(obj.chbuts(i), 'UserData');
                        % Handle case where UserData might be empty or incomplete
                        if isempty(userData) || ~isfield(userData, 'isDifferential')
                            % Single-ended channel (no differential info)
                            effectiveChannelCount = effectiveChannelCount + 1;
                            processedChannels(i) = true;
                        elseif userData.isDifferential && userData.pairChannel > 0
                            % This is a differential pair, count as 1
                            effectiveChannelCount = effectiveChannelCount + 1;
                            processedChannels(i) = true;
                            processedChannels(userData.pairChannel) = true;
                        else
                            % Single-ended channel
                            effectiveChannelCount = effectiveChannelCount + 1;
                            processedChannels(i) = true;
                        end
                    end
                end
                
                % Calculate sampling rate with constraints:
                % - Single channel: max 50kHz
                % - Multiple channels: 100kHz / N (shared bandwidth)
                if effectiveChannelCount == 0
                    obj.sampling = 12500;  % Default: 100kHz / 8
                elseif effectiveChannelCount == 1
                    obj.sampling = 50000;  % Single channel max
                else
                    % Multiple channels share 100kHz bandwidth
                    obj.sampling = floor(100000 / effectiveChannelCount);
                end
                
                enabledChannelNums = find(obj.channelEnabled) - 1;
                display(['--- Sampling Rate Info ---']);
                display(['Enabled channels: [' num2str(enabledChannelNums) ']']);
                display(['Effective channel count: ' num2str(effectiveChannelCount)]);
                display(['Per-channel sampling rate: ' num2str(obj.sampling) ' Hz']);
                display(['Total bandwidth usage: ' num2str(obj.sampling * effectiveChannelCount) ' Hz']);
                display(['------------------------']);
                
            catch ex
                display(['Error in updateChannelsAndSampling: ' ex.message]);
            end
        end
        
        function toggleDifferentialPair(obj, channelIndex)
            % Toggle differential mode for a channel pair
            % channelIndex is 1-based (1-8), corresponding to channels 0-7
            fprintf('\n=== toggleDifferentialPair called ===\n');
            fprintf('Input channelIndex (1-based): %d\n', channelIndex);
            
            chanNum = channelIndex - 1; % Convert to 0-based channel number
            fprintf('Channel number (0-based): %d\n', chanNum);
            
            if chanNum > 3
                msgbox('Only channels 0-3 can be set as differential (paired with 4-7)', 'Differential Mode');
                return;
            end
            
            lowIdx = channelIndex;      % 1-based index for low channel
            highIdx = channelIndex + 4;  % 1-based index for high channel
            fprintf('Low channel index: %d, High channel index: %d\n', lowIdx, highIdx);
            
            % Check if both channels in the pair are enabled
            fprintf('Channel %d enabled: %d\n', chanNum, obj.channelEnabled(lowIdx));
            fprintf('Channel %d enabled: %d\n', chanNum+4, obj.channelEnabled(highIdx));
            
            if ~obj.channelEnabled(lowIdx) || ~obj.channelEnabled(highIdx)
                msgbox(sprintf('Both channels %d and %d must be enabled for differential mode.', chanNum, chanNum+4), 'Differential Mode');
                return;
            end
            
            lowData = get(obj.chbuts(lowIdx), 'UserData');
            highData = get(obj.chbuts(highIdx), 'UserData');
            
            fprintf('Current differential state: %d\n', lowData.isDifferential);
            
            if ~lowData.isDifferential
                % Enable differential mode
                lowData.isDifferential = true;
                lowData.pairChannel = highIdx;
                highData.isDifferential = true;
                highData.pairChannel = lowIdx;
                
                % Update button labels and colors
                set(obj.chbuts(lowIdx), 'String', sprintf('%d-%d', chanNum, chanNum+4));
                set(obj.chbuts(lowIdx), 'BackgroundColor', lowData.diffColor);
                set(obj.chbuts(highIdx), 'String', sprintf('%d-%d', chanNum, chanNum+4));
                set(obj.chbuts(highIdx), 'BackgroundColor', highData.diffColor);
                
                % Hide the high channel in display (differential result shown on low channel)
                obj.channelVisible(highIdx) = false;
                if ~isempty(obj.fig_cache)
                    obj.fig_cache.channelVisible(highIdx) = false;
                end
                
                fprintf('差分模式已启用: 通道 %d-%d (结果显示在通道 %d)\n', chanNum, chanNum+4, chanNum);
            else
                % Disable differential mode
                lowData.isDifferential = false;
                lowData.pairChannel = -1;
                highData.isDifferential = false;
                highData.pairChannel = -1;
                
                % Restore button labels and colors
                set(obj.chbuts(lowIdx), 'String', sprintf('CH%d', chanNum));
                set(obj.chbuts(lowIdx), 'BackgroundColor', lowData.channelColor);
                set(obj.chbuts(highIdx), 'String', sprintf('CH%d', chanNum+4));
                set(obj.chbuts(highIdx), 'BackgroundColor', highData.channelColor);
                
                % Restore high channel visibility
                obj.channelVisible(highIdx) = true;
                if ~isempty(obj.fig_cache)
                    obj.fig_cache.channelVisible(highIdx) = true;
                end
                
                fprintf('差分模式已禁用: 通道 %d-%d\n', chanNum, chanNum+4);
            end
            
            set(obj.chbuts(lowIdx), 'UserData', lowData);
            set(obj.chbuts(highIdx), 'UserData', highData);
            
            fprintf('UserData updated for both channels\n');
            
            % Update sampling rate
            obj.updateChannelsAndSampling();
            fprintf('=== toggleDifferentialPair completed ===\n\n');
        end

        function sz = getPixelPos(~, hnd)
            old_units = get(hnd,'Units');
            set(hnd,'Units','Pixels');
            sz = get(hnd,'Position');
            set(hnd,'Units',old_units);
        end

        function in = parseInputs(obj, varargin)
            try
                % use inputParser to figure out all the inputs
                p = inputParser;
                varargin = varargin{:};

                % defaults and checks
                defaultSampleFreq = 50000;%4 channels
                checkSampleFreq = @(x) all([isnumeric(x), numel(x)==1, x>=10, x<=100000]);
                defaultChannels = [0, 1];
                checkChannels = @(x) all([isnumeric(x), arrayfun(@(y) y>=0, x), ...
                    arrayfun(@(y) y<=7, x), numel(unique(x))==numel(x), ...
                    numel(x)>0, numel(x)<=8]);
                defaultAlpha = [1, 1];
                checkAlpha = @(x) all([isnumeric(x), arrayfun(@(y) y>0, x), ...
                    numel(x)>0, numel(x)<=8]);
                % defaultOutputAlpha = 10;%10
                defaultOutputAlpha = 1;%10
                checkOutputAlpha = @(x) all([isnumeric(x), numel(x)==1, x>0]);

                % if channels are specified, make sure alphas and sample
                % frequencies have the right number of elements
                logic = cellfun(@(x) strcmp(x,'Channels'), varargin);
                if sum(logic)>0
                    ind = find(logic,1,'first'); % which index is the input 'Channels'
                    chan = varargin{ind+1}; % vector of channels
                    checkAlpha = @(x) all([isnumeric(x), arrayfun(@(y) y>0, x), numel(x)==numel(chan)]);
                    checkSampleFreq = @(x) all([isnumeric(x),x>=10,x<=floor(100000/numel(chan))]);
                end

                % set up the inputs
                addOptional(p,'SampleFrequency',defaultSampleFreq,checkSampleFreq);
                addOptional(p,'Channels',defaultChannels,checkChannels);
                addOptional(p,'OutputAlpha',defaultOutputAlpha,checkOutputAlpha);
                addOptional(p,'Alphas',defaultAlpha,checkAlpha);

                % parse
                parse(p,varargin{:});
                in = p.Results;
            catch ex
                display('Invalid inputs to DataAcquisition.')
                display('Valid name/value pair designations are:')
                display('''SampleFrequency''')
                display('''Channels''')
                display('''Alphas''')
                display('''OutputAlpha''')
                display('''Channels'' is a maximum 8 element vector containing the integers 0 through 7.  E.g. [0, 1, 2, 3]')
                display('Keep in mind that if you specify channels, you must specify the value of alpha for each of those channels.')
                display('Example of a valid call: >> DataAcquisition(''Channels'',[0,1],''Alphas'',[1,1])')
            end
        end

    end

end