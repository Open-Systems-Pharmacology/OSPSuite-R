require 'open-uri'
require 'openssl'

require_relative 'scripts/copy-dependencies'
require_relative 'scripts/utils'
require_relative 'scripts/R'
require_relative 'scripts/colorize'

OpenSSL::SSL::VERIFY_PEER = OpenSSL::SSL::VERIFY_NONE

APPVEYOR_ACCOUNT_NAME = 'open-systems-pharmacology-ci'

task :prepare_for_build, [:build_version, :pksim_branch] do |t, args|
  args.with_defaults(:pksim_branch => 'develop')
  update_package_version(args.build_version, description_file)
  copy_files_to_lib_folder
  install_pksim(args.pksim_branch)
end

task :postclean do 
  os = 'win'
  clear_folders
  nuget_restore os
  copy_files_to_lib_folder 
end

# This task is temporary until we have an automated linux build
task :create_linux_build, [:product_version, :build_dir, :linux_distro] do |t, args|
  product_version = sanitized_version(args.product_version)

  build_dir = args.build_dir
  linux_distro = args.linux_distro

  puts "Build dir is #{build_dir}".light_blue

  #TEMP
  build_dir = "C:/projects/ospsuite-r"

  #run nuget to get linux packages
  nuget_restore linux_distro

  # Tar file produced by the script
  tar_file_name = "ospsuite_#{product_version}.tar.gz"
  tar_file = File.join(build_dir, tar_file_name)
  puts "Windows package is #{tar_file}".light_blue

  #unzip it in a temp folder
  temp_distro_dir = File.join(temp_dir, linux_distro)
  FileUtils.mkdir_p temp_distro_dir

  command_line = %W[xzf #{tar_file} -C #{temp_distro_dir}]
  Utils.run_cmd('tar', command_line)

  ospsuite_dir = File.join(temp_distro_dir,  'ospsuite')
  inst_lib_dir = File.join(ospsuite_dir, 'inst', 'lib')

  #Remove the windows dll that should be replace by linux binaries
  delete_dll('OSPSuite.FuncParserNative', inst_lib_dir)
  delete_dll('OSPSuite.SimModelNative', inst_lib_dir)
  delete_dll('OSPSuite.SimModelSolver_CVODES', inst_lib_dir)

  #Copy the linux binaries
  copy_so('OSPSuite.FuncParser', linux_distro, inst_lib_dir)
  copy_so('OSPSuite.SimModel', linux_distro,  inst_lib_dir)
  copy_so('OSPSuite.SimModelSolver_CVODES', linux_distro,  inst_lib_dir)

  Dir.chdir(temp_distro_dir) do
    tar_archive_name = "ospsuite_#{product_version}_#{linux_distro}.tar"
    command_line = %W[cf #{tar_archive_name} ospsuite]
    Utils.run_cmd('tar', command_line)

    Utils.run_cmd('gzip', %W[#{tar_archive_name}])

    FileUtils.mv("#{tar_archive_name}.gz", build_dir)
  end
end


private
def copy_so(file, linux_distro, target_dir)
  native_folder = '/bin/native/x64/Release/'
  copy_dependencies packages_dir, target_dir do
    copy_files "#{file}.#{linux_distro}*/**/#{native_folder}", 'so'
    copy_files "#{file}.#{linux_distro}*/**/netstandard*", 'dll'
  end
end

def install_pksim(branch)
  file_name ='setup.zip'
  appveyor_project_name = 'pk-sim'
  uri = "https://ci.appveyor.com/api/projects/#{APPVEYOR_ACCOUNT_NAME}/#{appveyor_project_name}/artifacts/#{file_name}?branch=#{branch}"
  zip_package = download_file(appveyor_project_name, file_name, uri)
  msi_package = unzip_package(zip_package)
  # MSI installer only works with \\ style separator
  msi_package = msi_package.split('/').join('\\')
  puts "Installing #{msi_package} silently".light_blue
  command_line = %W[/i #{msi_package} /quiet /qn /norestart]
  Utils.run_cmd('msiexec.exe', command_line)
  puts "Installation done.".light_blue
end

def download_file(project_name, file_name, uri)
  download_dir = File.join(temp_dir, project_name) 
  FileUtils.mkdir_p download_dir
  file = File.join(download_dir, file_name)
  puts "Downloading #{file_name} from #{uri} under #{file}".light_blue
  open(file, 'wb') do |fo|
    fo.print URI.open(uri,:read_timeout => nil).read
  end
  file
end

def unzip_package(package_full_path)
  unzip_dir = unzip(package_full_path)
  artifact_name = ''
  Dir.glob(File.join(unzip_dir, '*.msi')) do |x|
    artifact_name = x
  end 
  artifact_name
end

def unzip(package_full_path)
  unzip_dir = File.dirname(package_full_path)
  command_line = %W[e #{package_full_path} -o#{unzip_dir}]
  Utils.run_cmd('7z', command_line)
  unzip_dir
end


def delete_dll(file, dir)
  file_full_path = File.join(dir, "#{file}.dll")
  File.delete(file_full_path)
end

def copy_files_to_lib_folder
  copy_packages_files
  copy_modules_files
end

def clear_folders
  FileUtils.rm_rf  lib_dir
  FileUtils.rm_rf  packages_dir
  FileUtils.mkdir_p lib_dir
  FileUtils.mkdir_p packages_dir
end

def copy_packages_files
  native_folder = '/bin/native/x64/Release/'
  copy_dependencies packages_dir, lib_dir do
    # Copy all netstandard dlls. The higher version will win (e.g. 1.6 will be copied after 1.5)
    copy_files '*/**/lib/netstandard2.0', 'dll'

    # Copy all x64 release dll and so from OSPSuite
    copy_files "OSPSuite.*#{native_folder}", ['dll', 'so']

    #special case for NPOI that does not work in .net standard mode
    copy_files 'NPOI*/**/net45', 'dll'
  end

end

def copy_modules_files
  copy_dependencies solution_dir, lib_dir do
    copy_dimensions_xml
    copy_pkparameters_xml
  end
end

def nuget_restore(os)
  command_line = %W[install packages.config -OutputDirectory packages]
  Utils.run_cmd('nuget', command_line)

  command_line = %W[install packages.#{os}.config -OutputDirectory packages]
  Utils.run_cmd('nuget', command_line)
end

def solution_dir
  File.dirname(__FILE__)
end

def inst_dir
  File.join(solution_dir,'inst')
end

def lib_dir
  File.join(inst_dir,'lib')
end

def packages_dir
  File.join(solution_dir,'packages')
end

def modules_dir
  File.join(solution_dir,'modules')
end

def description_file
  File.join(solution_dir,'DESCRIPTION')
end 

def temp_dir
  "C:/temp"
end