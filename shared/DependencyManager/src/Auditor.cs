namespace DependencyManager;

public static class Auditor
{
    /// <summary>
    /// Returns one record per loaded assembly: "Name|Version|Location".
    /// Location is empty for runtime/GAC-resolved assemblies; non-empty
    /// values point into inst/lib for assemblies we ship ourselves.
    /// Used by tests/testthat/teardown.R to audit which shipped DLLs
    /// are actually loaded during the R test suite. See issue #1587.
    /// </summary>
    public static string[] GetLoadedAssemblies()
    {
        return AppDomain.CurrentDomain.GetAssemblies()
            .Where(a => !a.IsDynamic)
            .Select(a =>
            {
                var name = a.GetName();
                var location = string.IsNullOrEmpty(a.Location) ? "" : a.Location;
                return $"{name.Name}|{name.Version}|{location}";
            })
            .OrderBy(s => s, StringComparer.OrdinalIgnoreCase)
            .ToArray();
    }
}
