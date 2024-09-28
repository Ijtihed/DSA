/** Copyright */

package plotters

import scala.collection.mutable.ArrayBuffer
import java.io.{File, FileWriter}

trait Plotters:
  case class Results(name: String, points: ArrayBuffer[(Int, Double)]) {}

  def writeHTML(title: String, filename: String, allResults: collection.Seq[Results]) =
    val out = FileWriter(File(filename))
    out.write("""<!DOCTYPE html>
<html lang="en">
<head>
<style>
body {font-size: 100%; }
button {font-size:1.2em;}
</style>
</head>
<body>
  <div><canvas id="myChart"></canvas></div>
  <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
  <script>
    data = [""")
    for results <- allResults do
      out.write(s"      {title: '${results.name}', points: [")
      out.write(
        results.points
          .map((x, y) => "{x: %d, y:%.9f}".formatLocal(java.util.Locale.US, x, y))
          .mkString(",")
      )
      out.write("]},\n")
    out.write(s"""    ];
    const ctx = document.getElementById('myChart');
    Chart.defaults.font.size = 20;
    const chart = new Chart(ctx, {
      type: 'line',
      borderWidth: 3,
      data: {
        datasets: []
      },
      options: {
        locale: 'en-us',
        plugins: {
          title: {
            text: "$title",
            display: true
          }
        },
        scales: {
          y: {
            //beginAtZero: true,
            type: 'linear',
            title: {
              text: 'time (s)',
              display: true,
            }
          },
          x: {
            //beginAtZero: true,
            type: 'linear',
            position: 'bottom',
            title: {
              text: 'n',
              display: true,
            }
          }
        }
      }
    });
    for (var i = 0; i < data.length; i++) {
      const d = {label: data[i].title, data: data[i].points };
      chart.data.datasets.push(d);
    }
    chart.update();
    function change_scale() {
      if(chart.options.scales.y.type == "linear")
        chart.options.scales.y.type = "logarithmic";
      else
        chart.options.scales.y.type = "linear";
      chart.update();
    };
    var units = "s";
    function change_unit() {
      if(units == "s") {
        units = "ms";
        for (var i = 0; i < data.length; i++) {
          chart.data.datasets[i].data = data[i].points.map(p => {return {x: p.x, y: p.y*1000}});
        }
        chart.options.scales.y.title.text = 'time (ms)';
      } else {
        units = "s";
        for (var i = 0; i < data.length; i++) {
          chart.data.datasets[i].data = data[i].points;
        }
        chart.options.scales.y.title.text = 'time (s)';
      }
      chart.update();
    };
    </script>
    <button onclick="change_scale();">use lin/log time scale</button>
    <button onclick="change_unit();">use seconds/milliseconds</button>
</body>
</html>
""")
    out.close()

  def writeJSON(title: String, filename: String, allResults: collection.Seq[Results]) =
    val out = FileWriter(File(filename))
    out.write(
      s"""{"title": "$title",
        "data": [
    """ + allResults
        .map(results =>
          s"""{"title": "${results.name}", "data": [""" + results.points
            .map((x, y) => "{\"x\":%d,\"y\":%.9f}".formatLocal(java.util.Locale.US, x, y))
            .mkString(",") + "]}"
        )
        .mkString(",\n") + """
    ]}"""
    )
    out.close()

  def writeCSV(filename: String, allResults: collection.Seq[Results]) =
    val delimiter = " | "
    val out = FileWriter(File(filename))
    val xCoords = allResults.head.points.map(_._1)
    require(allResults.forall(_.points.map(_._1) == xCoords))
    out.write(
      "n" + delimiter + allResults.map(_.name).mkString(delimiter) + "\n"
    )
    for (x, index) <- xCoords.zipWithIndex do
      out.write(
        x.toString + delimiter + allResults
          .map(r => "%.9f".formatLocal(java.util.Locale.US, r.points(index)._2))
          .mkString(delimiter) + "\n"
      )
    out.close()

  /** Writes filenameBase.gnuplot and filenameBase.csv.
    * Run gnuplot on filenameBase.gnuplot to create filenameBase.eps.
    */
  def writeGnuplot(
      title: String,
      filenameBase: String,
      allResults: collection.Seq[Results],
      yLogScale: Boolean
  ) =
    writeCSV(filenameBase + ".csv", allResults)
    val out = FileWriter(File(filenameBase + ".gnuplot"))
    val xLogScale = false
    out.write(
      s"""set term  postscript eps enhanced color font 'Helvetica,20'
set out '${filenameBase + ".eps"}'
set datafile separator '|'
set title '$title'
set key top left
set xtics font ", 16" center rotate by 20 offset 0,-0.3
""" + (if xLogScale then "set logscale x" else "") + """
""" + (if yLogScale then "set logscale y" else "") + """
set xlabel 'n'
set ylabel 'time(s)'
plot """ + allResults.zipWithIndex
.map((r, i) =>
  s"'${filenameBase + ".csv"}' using 1:${i + 2} with lines lw 6 title '${r.name}'"
)
.mkString(",") + """
quit
""")
    out.close()
