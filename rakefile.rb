require_relative 'scripts/copy-dependencies'
require_relative 'scripts/utils'

task :prepare_for_build, [:product_version] do |t, args|
  product_version = sanitized_version(args.product_version)
 
  copy_files_to_lib_folder

  update_package_version(product_version)
end

task :postclean do 
  os = 'win'
  clear_folders
  nuget_restore os
  copy_files_to_lib_folder 
end

# This task is temporary until we have an automated linux buold
task :create_linux_build, [:product_version, :build_dir, :linux_distro] do |t, args|
  product_version = sanitized_version(args.product_version)
  build_dir = args.build_dir
  linux_distro = args.linux_distro

  #run nuget to get linux packages
  nuget_restore linux_distro

  tar_file_name = "ospsuite_#{product_version}.tar.gz"
  
  # Tar file produced by the script
  tar_file = File.join(build_dir, tar_file_name)

  #unzip it in a temp folder
  temp_dir = File.join(build_dir, linux_distro)
  FileUtils.mkdir_p temp_dir

  command_line = %W[xzf #{tar_file} -C #{temp_dir}]
  Utils.run_cmd('tar', command_line)

  ospsuite_dir = File.join(temp_dir,  'ospsuite')
  inst_lib_dir = File.join(ospsuite_dir, 'inst', 'lib')

  #Remove the windows dll that should be replace by linux binaries
  delete_dll('OSPSuite.FuncParserNative', inst_lib_dir)
  delete_dll('OSPSuite.SimModelNative', inst_lib_dir)
  delete_dll('OSPSuite.SimModelSolver_CVODES', inst_lib_dir)

  #Copy the linux binaries
  copy_so('OSPSuite.FuncParser', linux_distro, inst_lib_dir)
  copy_so('OSPSuite.SimModel', linux_distro,  inst_lib_dir)
  copy_so('OSPSuite.SimModelSolver_CVODES', linux_distro,  inst_lib_dir)

  Dir.chdir(temp_dir) do
    tar_archive_name = "ospsuite_#{product_version}_#{linux_distro}.tar"
    command_line = %W[cf #{tar_archive_name} ospsuite]
    Utils.run_cmd('tar', command_line)

    Utils.run_cmd('gzip', %W[#{tar_archive_name}])

    FileUtils.mv("#{tar_archive_name}.gz", build_dir)
  end
end


private

def zip(command_line)
  Utils.run_cmd('7z', command_line) 
end

def copy_so(file, linux_distro, target_dir)
  native_folder = '/bin/native/x64/Release/'
  copy_depdencies packages_dir, target_dir do
    copy_files "#{file}.#{linux_distro}*/**/#{native_folder}", 'so'
    copy_files "#{file}.#{linux_distro}*/**/netstandard*", 'dll'
  end


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
  copy_depdencies packages_dir, lib_dir do
    # Copy all netstandard dlls. The higher version will win (e.g. 1.6 will be copied after 1.5)
    copy_files '*/**/netstandard*', 'dll'

    # Copy all x64 release dll and so from OSPSuite
    copy_files "OSPSuite.*#{native_folder}", ['dll', 'so']
  end

end

def copy_modules_files
  copy_depdencies solution_dir, lib_dir do
    copy_dimensions_xml
    copy_pkparameters_xml
  end
end

def sanitized_version(version) 
  pull_request_index = version.index('-')
  return version unless pull_request_index
  version.slice(0, pull_request_index)
end

def update_package_version(version) 
  #Replace token Version: x.y.z with the version from appveyor
  replacement = {
    /Version: \d\.\d\.\d/ => "Version: #{version}"
  }

  Utils.replace_tokens(replacement, description_file)
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