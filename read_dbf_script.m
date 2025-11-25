% 读取 DataAcquisition 中保存的 DBF 文件
% 此脚本使用 dbfload.m 函数来读取和分析 .dbf 格式的数据文件

% 清空工作区
clear;
clc;

%% 1. 选择要读取的 DBF 文件
% 方法1：手动选择文件
[filename, pathname] = uigetfile('*.dbf', '选择要读取的 DBF 文件');
if isequal(filename, 0)
    disp('用户取消了文件选择');
    return;
end
fullpath = fullfile(pathname, filename);

% 方法2：直接指定文件路径（取消注释以使用）
% fullpath = 'y:\dengtao\DA2\DataAcquisition\2023_06_17_0001.dbf';

fprintf('正在读取文件: %s\n', fullpath);

%% 2. 首先读取文件头信息
[~, header] = dbfload(fullpath, 'info');

% 显示文件头信息
fprintf('\n==================== 文件信息 ====================\n');
fprintf('采样间隔: %.6f 秒 (采样频率: %.2f Hz)\n', header.si, 1/header.si);
fprintf('通道数量: %d\n', header.numChan);
fprintf('每通道数据点数: %d\n', header.numPts);
fprintf('总数据点数: %d\n', header.numTotal);
fprintf('数据压缩比例因子: [%s]\n', num2str(header.data_compression_scaling));
fprintf('\n通道名称:\n');
for i = 1:length(header.chNames)
    fprintf('  通道 %d: %s\n', i, header.chNames{i});
end
fprintf('==================================================\n\n');

%% 3. 读取数据选项
% 用户选择读取方式
choice = questdlg('选择数据读取方式:', ...
    '数据读取选项', ...
    '全部数据', '指定范围', '仅头信息', '全部数据');

switch choice
    case '全部数据'
        % 读取全部数据（注意：大文件可能需要较长时间）
        fprintf('正在读取全部数据...\n');
        tic;
        [data, ~] = dbfload(fullpath);
        elapsed_time = toc;
        fprintf('数据读取完成！耗时: %.2f 秒\n', elapsed_time);
        
    case '指定范围'
        % 读取指定范围的数据
        prompt = {sprintf('起始点 (0-%d):', header.numPts-1), ...
                  sprintf('结束点 (0-%d):', header.numPts-1)};
        dlgtitle = '输入数据范围';
        dims = [1 35];
        definput = {'0', num2str(min(10000, header.numPts))};
        answer = inputdlg(prompt, dlgtitle, dims, definput);
        
        if isempty(answer)
            disp('用户取消了范围输入');
            return;
        end
        
        range_start = str2double(answer{1});
        range_end = str2double(answer{2});
        range = [range_start, range_end];
        
        fprintf('正在读取数据范围 [%d, %d]...\n', range_start, range_end);
        tic;
        [data, ~] = dbfload(fullpath, range);
        elapsed_time = toc;
        fprintf('数据读取完成！耗时: %.2f 秒\n', elapsed_time);
        
    case '仅头信息'
        fprintf('仅读取了文件头信息。\n');
        return;
        
    otherwise
        disp('取消操作');
        return;
end

%% 4. 显示数据信息
fprintf('\n==================== 数据信息 ====================\n');
fprintf('数据矩阵大小: %d 行 x %d 列\n', size(data, 1), size(data, 2));
fprintf('数据类型: %s\n', class(data));
fprintf('数据范围:\n');
for i = 1:size(data, 2)
    fprintf('  %s: [%.4f, %.4f]\n', header.chNames{i}, min(data(:,i)), max(data(:,i)));
end
fprintf('==================================================\n\n');

%% 5. 创建时间轴
time = (0:size(data,1)-1)' * header.si;

%% 6. 绘制数据
fprintf('正在绘制数据...\n');
figure('Name', ['DBF 数据: ' filename], 'NumberTitle', 'off');

% 根据通道数量决定子图布局
numChannels = size(data, 2);
if numChannels <= 4
    rows = numChannels;
    cols = 1;
else
    rows = ceil(numChannels / 2);
    cols = 2;
end

% 为每个通道绘制子图
for i = 1:numChannels
    subplot(rows, cols, i);
    plot(time, data(:,i), 'LineWidth', 0.5);
    xlabel('时间 (秒)');
    ylabel(header.chNames{i});
    title(sprintf('通道 %d: %s', i, header.chNames{i}));
    grid on;
end

% 添加总标题
sgtitle(sprintf('DBF 文件数据: %s', filename), 'Interpreter', 'none');

fprintf('绘图完成！\n');

%% 7. 保存到工作区
% 将数据和头信息保存到基础工作区
assignin('base', 'dbf_data', data);
assignin('base', 'dbf_header', header);
assignin('base', 'dbf_time', time);
fprintf('\n数据已保存到工作区变量:\n');
fprintf('  dbf_data   - 数据矩阵 [%d x %d]\n', size(data, 1), size(data, 2));
fprintf('  dbf_header - 文件头信息结构体\n');
fprintf('  dbf_time   - 时间轴向量\n');

%% 8. 可选：导出数据
export_choice = questdlg('是否导出数据到其他格式?', ...
    '导出选项', ...
    'MAT文件', 'CSV文件', '不导出', '不导出');

switch export_choice
    case 'MAT文件'
        [save_file, save_path] = uiputfile('*.mat', '保存 MAT 文件', ...
            [pathname, filename(1:end-4), '.mat']);
        if ~isequal(save_file, 0)
            save(fullfile(save_path, save_file), 'data', 'header', 'time');
            fprintf('数据已保存到: %s\n', fullfile(save_path, save_file));
        end
        
    case 'CSV文件'
        [save_file, save_path] = uiputfile('*.csv', '保存 CSV 文件', ...
            [pathname, filename(1:end-4), '.csv']);
        if ~isequal(save_file, 0)
            % 创建表格并保存
            T = array2table([time, data], 'VariableNames', ['Time', header.chNames]);
            writetable(T, fullfile(save_path, save_file));
            fprintf('数据已保存到: %s\n', fullfile(save_path, save_file));
        end
end

fprintf('\n脚本执行完成！\n');
