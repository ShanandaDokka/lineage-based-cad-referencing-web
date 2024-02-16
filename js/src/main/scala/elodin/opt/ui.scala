package elodin.opt

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.{html, document, window, Element, MouseEvent}
import org.scalajs.dom.raw.{Event, KeyboardEvent, Node}
import typings.monaco.monaco
import typings.monaco.monaco.editor
import typings.monaco.{CreateEditorOptions, MinimapOptions, IColors, IThemeData}
import org.scalajs.dom.raw.{CSSStyleDeclaration, HTMLElement, HTMLFormElement, HTMLInputElement}

import collection.mutable.Buffer

import elodin.dom.api.*
import elodin.dom.svg.*
import elodin.geometry.Point

import elodin.dom.api.tags.*
import elodin.opt.dsl.ParamBounds

object UI:
  given Domable[HaloCircle] = c => c.root.root

  /** Wrapper class to create an inner and outer circle as part of a group. This is useful when we want a
    * clickbox larger than the visible area.
    */
  case class HaloCircle(var position: (Double, Double), var radius: Double)(using SVGContext):
    val root = Group(
      (Circle(position, radius), Circle(position, radius / 2))
    )
    val (outer, inner) = root.contents

    def update(p: (Double, Double)) =
      position = p
      outer.setPosition(p)
      inner.setPosition(p)

  def hoverStyle(pt: (Double, Double), circle: HaloCircle, visible: Boolean)(using SVGContext) =
    circle.outer.attr("fill" -> "transparent")
    circle.inner.attr("fill" -> "rgba(128,0,0,0.5)")
  // circle
  //   .withHover(circle.position, s"${pt}")
  // .listen("click", _ => window.navigator.clipboard.writeText(s"\"${pt.id}\""))

  def displayCode(element: Element)(content: String) =
    val spaces = " " * 6
    val processed = content
      .split("\n")
      .toSeq
      .map(s => s"<div>  ${s.stripPrefix(spaces)}</div>")
      .mkString("\n")
      .replace(" ", "&nbsp;")

    element(
      code("scala-source") / processed
    )

  def dropdown(name: String, args: Seq[(String, () => Any)]) =
    val s = tag("select")(
      args.map((a, _) => tag("option").attr("value" -> a) / a)
    )
    s.addEventListener("change", _ => if s.selectedIndex >= 0 then args(s.selectedIndex)(1)())
    div(
      // label.attr("for" -> name) / s"$name:",
      s
    )

  class RenderMode(var state: View, onChange: View => Unit):
    import View.*

    val buttonText = p / stateToText(state)
    val button = div("mode-button")(buttonText)

    def stateToText(view: View) =
      view match
        case View2D => "3D"
        case _      => "2D"

    def setStateNoCallback(view: View) =
      buttonText.innerText = stateToText(view)
      state = view

    def setView(view: View) =
      setStateNoCallback(view)
      onChange(state)

    button.listen(
      "click",
      _ =>
        setView(state match
          case View2D => View3D
          case _      => View2D
        )
    )

  class GenericButton(text: String):
    val subscribers = Buffer[() => Unit]()

    def listen(callback: () => Unit) =
      subscribers += callback

    val button = div("mode-button")(p / text)
    button.listen(
      "click",
      _ => for action <- subscribers do action()
    )

  class InputButton(text: String):
    val subscribers = collection.mutable.Buffer[() => Unit]()

    def listen(callback: () => Unit): Unit =
      subscribers += callback

    val button = div("mode-button")(p / text)
    var popupIsVisible = false
    var popupContainer = div("example-menu")
    var active = false
    var programText = ""
    var popupHtml = ""
    var createdObject = ""

    def showPopUp(editor: DynamicEditor, objectNum: Integer, onFormSubmit: () => Unit): Unit = 
      if (text == "drawRectangle") {
        popupHtml =
        s"""
          |<div style="display: flex; align-items: center;">
          |  <form id="popupForm" style="background-color: #f4f4f4; padding: 20px; border-radius: 8px;">
          |    <label for="x">Point X:</label>
          |    <input type="number" id="x" name="x" placeholder="X" style="width: 40px" required><br>
          |    <label for="y">Point Y:</label>
          |    <input type="number" id="y" name="y" placeholder="Y" style="width: 40px" required><br>
          |    <label for="height">Height:</label>
          |    <input type="number" id="height" name="height" style="width: 40px" required><br>
          |    <label for="width">Width:</label>
          |    <input type="number" id="width" name="width" style="width: 40px" required><br>
          |    <input type="submit" value="Submit">
          |  </form>
          |</div>
          |""".stripMargin
      } else if (text == "drawCircle") {
          popupHtml =
          s"""
            |<div style="display: flex; align-items: center;">
            |  <form id="popupForm" style="background-color: #f4f4f4; padding: 20px; border-radius: 8px;">
            |    <label for="x">CircCen X:</label>
            |    <input type="number" id="x" name="x" placeholder="X" style="width: 40px" required><br>
            |    <label for="y">CircCen Y:</label>
            |    <input type="number" id="y" name="y" placeholder="Y" style="width: 40px" required><br>
            |    <label for="radius">Radius:</label>
            |    <input type="number" id="radius" name="radius" style="width: 40px" required><br>
            |    <input type="submit" value="Submit">
            |  </form>
            |</div>
            |""".stripMargin
      } else if (text == "extrude3D") {
        popupHtml =
        s"""
          |<div style="display: flex; align-items: center;">
          |  <form id="popupForm" style="background-color: #f4f4f4; padding: 20px; border-radius: 8px;">
          |    <label for="variable">var:</label>
          |    <input type="string" id="variable" name="variable" style="width: 40px" required><br>
          |    <label for="length">Length:</label>
          |    <input type="number" id="length" name="length" style="width: 40px" required><br>
          |    <input type="submit" value="Submit">
          |  </form>
          |</div>
          |""".stripMargin
      }

      popupContainer.innerHTML = popupHtml

      val form = popupContainer.querySelector("#popupForm").asInstanceOf[HTMLFormElement]

      if (text == "drawRectangle") {
        form.addEventListener(
          "submit",
          (event: dom.Event) => {
            event.preventDefault()
            val xInput = form.querySelector("#x").asInstanceOf[HTMLInputElement]
            val yInput = form.querySelector("#y").asInstanceOf[HTMLInputElement]
            val heightInput = form.querySelector("#height").asInstanceOf[HTMLInputElement]
            val widthInput = form.querySelector("#width").asInstanceOf[HTMLInputElement]

            val x = xInput.value.toInt
            val y = yInput.value.toInt
            val height = heightInput.value.toInt
            val width = widthInput.value.toInt

            programText = s"r$objectNum = Rectangle(pt($x, $y), $height, $width)" + "\n" + s"draw(r$objectNum)"
            createdObject = s"r$objectNum"
            print("TEXT HERE " + text)

            onFormSubmit()

            deactivate()
          }
        )
      } else if (text == "drawCircle") {
        form.addEventListener(
          "submit",
          (event: dom.Event) => {
            event.preventDefault()
            val xInput = form.querySelector("#x").asInstanceOf[HTMLInputElement]
            val yInput = form.querySelector("#y").asInstanceOf[HTMLInputElement]
            val radiusInput = form.querySelector("#radius").asInstanceOf[HTMLInputElement]

            val x = xInput.value.toInt
            val y = yInput.value.toInt
            val radius = radiusInput.value.toInt

            programText = s"c$objectNum = Circle(pt($x, $y), $radius))" + "\n" + s"draw(c$objectNum)"
            createdObject = s"c$objectNum"

            onFormSubmit()

            deactivate()
          }
        )
      } else if (text == "extrude3D") {
        form.addEventListener(
          "submit",
          (event: dom.Event) => {
            event.preventDefault()
            val lengthInput = form.querySelector("#length").asInstanceOf[HTMLInputElement]
            val length = lengthInput.value.toInt

            val variableInput = form.querySelector("#variable").asInstanceOf[HTMLInputElement]
            val variable = variableInput.value.toString

            programText = s"e$objectNum = Extrude3D($variable, $length)" + "\n" + s"draw(e$objectNum)"
            createdObject = s"e$objectNum"

            onFormSubmit()

            deactivate()
          }
        )
      }
      
      val buttonRect = button.getBoundingClientRect()
      val style = popupContainer.style.asInstanceOf[CSSStyleDeclaration]
      style.position = "absolute"
      style.top = s"${buttonRect.bottom}px"
      style.left = s"${buttonRect.left}px"
    
    def activate() =
      document.body(popupContainer)
      active = true

    def deactivate() =
      document.body.removeChild(popupContainer)
      active = false

    document.body.addEventListener(
      "keydown",
      (e: KeyboardEvent) => if e.key == "Escape" && active then deactivate()
    )
    button.listen(
      "click",
      (event: dom.MouseEvent) => {
        event.preventDefault()
        if (!active) {
          activate()
        } else {
          deactivate()
        }

        for (action <- subscribers) {
          action()
        }
      }
    )

  class HelpDialog():
    val sc = "<code>"
    val ec = "</code>"
    val helpText = div("help-text")(
      p / s"- Press ${sc}ctrl/cmd${ec}+${sc}s${ec} to run.",
      p / s"- Switch examples using the dropdown in the top left. Quickly comment and un-comment lines in the editor with ${sc}ctrl/cmd${ec}+${sc}/$ec.",
      p / "- Zoom in with middle mouse button, pan by clicking and dragging right mouse button.",
      p / "- <strong>Switch the editor mode to 3D</strong> and 2D by toggling the 2D/3D switch.",
      p / "- Add a new parameter to the parameters block and rerun the program to create a new slider.",
      p / "- Edits are <strong>not persisted</strong> between changing dropdowns; please save changes externally."
    )
    val buttonIcon = p / "?"
    val button = div("mode-button")(buttonIcon)
    var active = false

    def activate() =
      document.body(helpText)
      buttonIcon.innerHTML = "X"
      active = true

    def deactivate() =
      document.body.removeChild(helpText)
      buttonIcon.innerHTML = "?"
      active = false

    button.listen("click", _ => if !active then activate() else deactivate())
    document.body.addEventListener(
      "keydown",
      (e: KeyboardEvent) => if e.key == "Escape" && active then deactivate()
    )

  class ErrorDialog(wrapper: HTMLElement):
    val dialog = div("error-dialog")
    wrapper.appendChild(dialog)

    def setText(text: String) =
      dialog.innerHTML = ""
      // val deleteButton = div("error-x") / "X"
      // deleteButton.listen("click", _ => delete())
      // dialog(deleteButton)
      dialog(p / text)

    private var isDeleted = false
    def delete() =
      if !isDeleted then
        wrapper.removeChild(dialog)
        isDeleted = true

  def displayEditableCode(parent: Element)(onChange: String => Unit, resized: () => Unit)(
      content: String
  ) =
    val editorElement =
      tag("div").attr(
        "style" -> "height: 80vh; right: 0px; border: 1px solid rgba(0,0,0,0.25);"
      )
    parent(div("editor-frame")(editorElement))

    var getText = () => ""

    def setParentWidth(width: Int) =
      EDITOR_WIDTH = width
      val px = s"${EDITOR_WIDTH}px"
      parent.withStyle("min-width" -> px, "max-width" -> px)
      resized()

    // The parent element needs to be rendered in the DOM in ord er for
    // monaco to be able to correctly read the size and positionitself.
    var editor: monaco.ICodeEditor = null
    window.setTimeout(
      () => {
        val codeEditor = monaco.editor.create(
          editorElement,
          new:
            value = content
            language = "scala"
            automaticLayout = true
            minimap = new MinimapOptions:
              enabled = false
            lineNumbersMinChars = 3
        )
        monaco.editor.defineTheme(
          "translucent",
          new IThemeData:
            var base = "vs"
            var inherit = true
            var rules = js.Array()
            var colors = new IColors:
              background = "#FFFFFF88"
        )
        monaco.editor.setTheme("translucent")
        val model = codeEditor.getModel()
        getText = () => model.getLinesContent().mkString("\n")
        codeEditor.onKeyDown((e: monaco.IKeyboardEvent) =>
          if (e.keyCode == 49) && (e.ctrlKey || e.metaKey) then // CTRL/CMD+S
            onChange(model.getLinesContent().mkString("\n"))
            e.preventDefault()
            e.stopPropagation()
        )
        editor = codeEditor
      },
      0
    )

    setParentWidth(EDITOR_WIDTH)
    val resizeBar = div("pc-bar")
    parent(resizeBar)
    resizeBar.listen(
      "mousedown",
      (e: MouseEvent) =>
        val mx = e.clientX
        document.body.withStyle("cursor" -> "ew-resize")
        document.body.listenOnce(
          "mouseup",
          (e: MouseEvent) =>
            val dx = e.clientX
            document.body.withStyle("cursor" -> "unset")
            setParentWidth(EDITOR_WIDTH - (dx - mx).toInt)
        )
    )

    () => getText()

  /** Renders the display into `container`. Invariants:
    *   - `.parameters` contains all of the current parameter nodes
    *   - `.currentValues` contains all of the current variables
    */
  class ParameterDisplay(container: Element, runProgram: Vector[Var] => Unit):
    val display = div("parameter-display")
    var parameters = Buffer[Parameter]()
    var currentValues = Vector[Var]()
    container(display)

    def updateDisplay(vars: Vector[Var]) =
      // assert(parameters.size == vars.size)
      parameters.zip(vars).map((d, v) => d.updateDisplay(v.value))
      currentValues = vars

    // Detect any parameters that have been "dirtied" by the UI (namely, changed by
    // the slider from the default value specified in the program text) and patch
    // their dirty values into the new parameter display generated on text change.
    def patchParameters(
        vars: Vector[Var]
    ): this.type =
      if vars.length == 0 then return this
      val maxChars = vars.map(_.name.length).reduce(math.max).toInt
      val onUpdate = () => runProgram(vars.withValues(parameters.map(_.currentValue).toSeq))
      val newParameters = Buffer[Parameter]()

      display.innerHTML = ""
      // println(s"Patching vars: ${vars.map(_.showDef).mkString(", ")}")
      currentValues = vars.map(v =>
        val p = Parameter(v.name, ParamBounds(v.min, v.value, v.max), maxChars, onUpdate)
        var value = v.value
        parameters.find(o => o.name == v.name).map { oldP =>
          if oldP.currentValue != oldP.defaultValue then
            val dirtyValue = clamp(oldP.currentValue, v.min, v.max)
            p.updateDisplay(dirtyValue)
            value = dirtyValue
        }
        newParameters += p
        display(p.node)
        v.withValue(value)
      )
      parameters.clear()
      parameters ++= newParameters
      this

  inline def clamp(value: Double, min: Double, max: Double) = math.max(min, math.min(max, value))

  class Parameter(val name: String, initialValue: dsl.ParamBounds, textWidth: Int, onUpdate: () => Unit):
    def fmtValue(value: Double) = f"${value}%.1f"
    private def %(value: Double) =
      val boundValue = clamp(value, minValue, maxValue)
      (boundValue - minValue) / (maxValue - minValue)

    // VALUES
    val sliderWidth = 500
    val defaultValue = initialValue.mid
    val minValue = initialValue.min
    var currentValue = initialValue.mid
    val maxValue = initialValue.max

    // COMPONENTS
    val textBox = input.attr("type" -> "text")
    val knob = div("parameter-clickbox")(div("parameter-knob"))
    val slider = div("parameter-slider")

    val node = div("parameter")(
      div("parameter-name").attr("style" -> s"width: ${textWidth}ch;") / name,
      textBox,
      slider.attr("style" -> s"width: ${sliderWidth}px;")(knob, div("parameter-ticks")),
      div("parameter-reset")
    )

    // BEHAVIOUR
    /** Set this parameter to a new percentage */
    private def update(
        percentage: Double,
        text: Boolean = true,
        recompute: Boolean = true
    ) =
      val offset = percentage * sliderWidth
      knob.attr("style" -> s"left: ${offset}px;")
      val value = minValue + (percentage * (maxValue - minValue))
      if text then textBox.value = fmtValue(value)
      currentValue = value
      if recompute then onUpdate()

    def updateDisplay(value: Double) =
      update(%(value), text = true, recompute = false)

    textBox.listen(
      "input",
      _ =>
        val s = textBox.value
        s.toDoubleOption.map(d => update(%(d), text = false, recompute = true))
        textBox.value = s
    )

    slider.listen(
      "mousedown",
      (e: MouseEvent) =>
        val bbox = slider.getBoundingClientRect()
        val (l, r) = (bbox.left, bbox.right)
        val percentage = (e.clientX - l) / (r - l)
        update(percentage, true, true)
    )

    applyDrag(
      knob,
      dragX(%(currentValue), 500, p => update(clamp(p, 0.0, 1.0)))
    )

    updateDisplay(initialValue.mid)

def dragX(percentage: => Double, sliderWidth: Double, newPercentage: Double => Unit): DraggingBehavior =
  val getPercentage = () => percentage
  var startX: Double = 0.0
  var startPercentage: Double = 0.0
  new DraggingBehavior:
    onStart = (x, _) =>
      startX = x
      startPercentage = getPercentage()
    onMove = (x, _) => newPercentage(startPercentage + ((x - startX) / sliderWidth))

trait DraggingBehavior:
  var onStart: (Double, Double) => Any = (x, y) => ()
  var onMove: (Double, Double) => Any = (x, y) => ()
  var onEnd: (Double, Double) => Any = (x, y) => ()

def applyDrag[T: Domable](target: T, behavior: DraggingBehavior) =
  var dragging = false
  var stopMoving = () => ()

  val moveHandler: js.Function1[MouseEvent, Any] = (e: MouseEvent) =>
    if dragging then
      val currentX = e.clientX; val currentY = e.clientY;
      behavior.onMove(currentX, currentY)

  val endHandler: js.Function1[MouseEvent, Any] = (e: MouseEvent) =>
    if dragging then
      val endX = e.clientX; val endY = e.clientY
      behavior.onEnd(endX, endY)
      dragging = false
      stopMoving()

  val startHandler: js.Function1[MouseEvent, Any] = (e: MouseEvent) =>
    if !dragging then
      val startX = e.clientX; val startY = e.clientY
      behavior.onStart(startX, startY)
      dragging = true
      stopMoving = document.body.listen("mousemove", moveHandler)
      document.body.listenOnce("mouseup", endHandler)
    else // Something went horribly wrong.
      stopMoving()
      dragging = false

  target.listen("mousedown", startHandler)
