require_relative 'scripts/copy-dependencies'
require_relative 'scripts/utils'

task :prepare_for_build,[:product_version] do |t, args|
  product_version = sanitized_version(args.product_version)
 
  copy_files_to_lib_folder

  update_package_version(product_version)
end

task :postclean do 
  clear_folders
  nuget_restore 'win'
  copy_files_to_lib_folder
end

private

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
    # Copy all x64 release dll from OSPSuite
    copy_files "OSPSuite.*#{native_folder}", 'dll'
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