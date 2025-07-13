#define NOMINMAX
#include <windows.h>

#include <map>
#include <tuple>
#include <memory>
#include <coroutine>
#include <cassert>
#include <array>
#include <bit>
#include <thread>
#include <vector>
#include <mmsystem.h>
#include <cmath>
#include <limits>

#pragma comment(lib, "winmm.lib")

template <typename T>
constexpr T max_of(const T&) {
    return std::numeric_limits<T>::max();
}

class BeepControl {
    static constexpr int sampleRate = 44000;       // 44kHz
    static constexpr int durationMs = 100;       // 0.2秒
    static constexpr int samples = sampleRate * durationMs / 1000;
    static constexpr double frequency = 880.0;    // A5
    std::vector<BYTE> buffer_;
    HWAVEOUT hWaveOut_;
    WAVEHDR hdr_ = {};
public:
    void Play() {
        if (hdr_.dwFlags & (WHDR_PREPARED | WHDR_DONE)) {
            waveOutWrite(hWaveOut_, &hdr_, sizeof(hdr_));
        }
    }
    BeepControl()
        : buffer_(samples) {
        for (int i = 0; i < samples; ++i) {
            double t = static_cast<double>(i) / sampleRate;
            t = sqrt(t); // piyo
            buffer_[i] = static_cast<BYTE>(127.5 * (sin(2 * 3.141592 * frequency * t) * 0.1 + 1));
        }
        WAVEFORMATEX wfx = {};
        wfx.wFormatTag = WAVE_FORMAT_PCM;
        wfx.nChannels = 1;
        wfx.nSamplesPerSec = sampleRate;
        wfx.wBitsPerSample = 8;
        wfx.nBlockAlign = wfx.nChannels * wfx.wBitsPerSample / 8;
        wfx.nAvgBytesPerSec = wfx.nSamplesPerSec * wfx.nBlockAlign;
        waveOutOpen(&hWaveOut_, WAVE_MAPPER, &wfx, 0, 0, CALLBACK_NULL);
        hdr_.lpData = reinterpret_cast<LPSTR>(buffer_.data());
        hdr_.dwBufferLength = samples;
        waveOutPrepareHeader(hWaveOut_, &hdr_, sizeof(hdr_));
    }
    ~BeepControl() {
        waveOutUnprepareHeader(hWaveOut_, &hdr_, sizeof(hdr_));
        waveOutClose(hWaveOut_);
    }
};

class Window; // Forward declaration

class HInstance {
    HINSTANCE hInstance_;
public:
    HINSTANCE Handle() const { return hInstance_; }
    HInstance(HINSTANCE hInstance) :
        hInstance_(hInstance) {
    }
};

std::unique_ptr<HInstance> g_hInstance;

#define TIMER_FRAME 1

class Ball {
    POINT pos_;
    POINT velocity_;
	int speed_ = 5; // ボールの速度
    static constexpr int shift = 4; // 固定小数点のシフト量
    static constexpr SIZE size_{ 10<<shift, 10<<shift }; // サイズは10x10ピクセル
public:
    void Move() {
        pos_.x += velocity_.x*speed_;
        pos_.y += velocity_.y*speed_;
    }
    bool Bound(int top, int bottom) {
		top <<= shift;
        bottom <<= shift;
        // 上下の壁に当たった場合、反転させて位置を調整
		// pos_.yは固定小数点なので、topとbottomも同様にシフトして比較する
		bool bound = false;
        if (pos_.y < top) {
            velocity_.y = -velocity_.y;
            pos_.y = top - (pos_.y - top);
			bound = true;
        }
        if (bottom <= pos_.y + size_.cy) {
            velocity_.y = -velocity_.y;
            auto wall = bottom - size_.cy;
            pos_.y = wall - (pos_.y - wall);
			bound = true;
		}
		return bound;
	}
    bool BoundPaddle(int y, int h) {
        y <<= shift;
        h <<= shift;
        if (pos_.y + size_.cy < y || y + h <= pos_.y) {
            return false; // パドルの範囲外
        }
        velocity_.x = -velocity_.x;
        velocity_.y = ((pos_.y + pos_.y + size_.cy - y-y - h) << shift) / h; // パドルの中央に合わせて速度を調整
		return true; // パドルに当たった
    }
    bool BoundLeft(int x, int y, int h) {
		if (!(velocity_.x < 0)) return false; // 左の壁に当たるのは左方向に移動しているときのみ
        x <<= shift;
        if (!(pos_.x < x)) return false;
        if (!BoundPaddle(y, h)) {
			return false; // パドルの範囲外
        }
        // パドルに当たった
        pos_.x = x - (pos_.x - x);
        return true;
	}
    bool BoundRight(int x, int y, int h) {
		if (!(0 < velocity_.x)) return false; // 右の壁に当たるのは右方向に移動しているときのみ
        x <<= shift;
		if (!(x < pos_.x)) return false;
        if (!BoundPaddle(y, h)) {
            return false; // パドルの範囲外
        }
        // パドルに当たった
        pos_.x = x - (pos_.x - x);
        return true;
    }
    const POINT FieldPosition() const { return POINT{ pos_.x >> shift,pos_.y >> shift }; }
    //POINT& Position() { return pos_; }
    //POINT& Velocity() { return velocity_; }
    static SIZE FieldSize() { return SIZE{ size_.cx >> shift, size_.cy >> shift }; }
    void SpeedUp() {
        speed_++;
	}
    Ball(POINT field_pos, int field_velocity)
        : pos_{field_pos.x<<shift, field_pos.y<<shift}
        , velocity_{ 1 << shift, 1 << shift }
		, speed_(field_velocity) {
    }
};

class Paddle {
    int y_;
    static constexpr SIZE size_ = { 5, 70 };
	int y_max_ = 0; // 最大Y座標
public:
    void Move(int delta_y) {
        y_ += delta_y;
        if (y_ < 0) y_ = 0;
		if (y_ > y_max_) y_ = y_max_;
    }
    int GetY() const { return y_; }
    static constexpr const SIZE& GetSize() { return size_; }
    Paddle(int y, int h) : y_(y), y_max_(h - size_.cy) {}
};

class Field {
    static constexpr SIZE size_ = { 800, 600 };
public:
    static constexpr const SIZE& GetSize() { return size_; }
    static int GetWidth() { return size_.cx; }
    static int GetHeight() { return size_.cy; }
	static int Top() { return 5; }
    static int Bottom() { return size_.cy - 5; }
    static int Left() { return 0; }
	static int Right() { return size_.cx; }
};

class Window {
    HWND handle_;
    LPCWSTR className_ = nullptr;
    std::atomic_bool should_close_ = false;
    static ::LRESULT CALLBACK DummyWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
        switch (msg) {
        case WM_SETFOCUS:
            // 親ウィンドウにフォーカスを戻す
            HWND hwndParent = GetParent(hwnd);
            if (hwndParent) {
                SetFocus(hwndParent);
            }
        }
        return DefWindowProc(hwnd, msg, wParam, lParam);
    }
    void Initialized() {
        SetWindowLongPtr(
            handle_,
            GWLP_USERDATA,
            reinterpret_cast<LONG_PTR>(this)
        );
    }
public:
    void TopMost() const {
        SetWindowPos(handle_, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
    }
    void SetWindowProc(WNDPROC wndProc) {
        Initialized();
        SetWindowLongPtr(handle_, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(wndProc));
    }
    void ShouldClose(bool should_close) {
        should_close_ = should_close;
    }
    bool ShouldClose() const { return should_close_; }
    HWND Handle() const { return handle_; }
    BOOL Show(int cmd) {
        return ShowWindow(handle_, cmd);
    }
    UINT_PTR SetTimer(UINT_PTR nIDEvent, UINT uElapse, TIMERPROC lpTimerFunc) {
        return ::SetTimer(handle_, nIDEvent, uElapse, lpTimerFunc);
    }
    BOOL SetPos(
        const Window* window_insert_after,
        int x, int y,
        int cx, int cy,
        UINT flags) {
        return ::SetWindowPos(
            handle_,
            window_insert_after ? window_insert_after->Handle() : NULL,
            x, y, cx, cy,
            flags
        );
    }
    BOOL InvalidateRect(const RECT* lpRect, BOOL bErase) {
        return ::InvalidateRect(handle_, lpRect, bErase);
    }
    BOOL Update() {
        return ::UpdateWindow(handle_);
    }
    BOOL MoveWindow(
        int X,
        int Y,
        int nWidth,
        int nHeight,
        BOOL bRepaint = TRUE
    ) {
        return ::MoveWindow(handle_, X, Y, nWidth, nHeight, bRepaint);
    }
    BOOL MoveWindowBlox(
        int X,
        int Y,
        int nWidth,
        int nHeight,
        BOOL bRepaint = TRUE
    ) {
        return ::MoveWindow(
            handle_,
            X, Y,
            nWidth, nHeight,
            bRepaint
        );
    }
    Window(const Window&) = delete;
    Window(
        LPCWSTR className,
        int brush,
        DWORD ws,
        SIZE size,
        Window* window_parent
    ) : handle_([](
        LPCWSTR className,
        int brush,
        DWORD ws,
        SIZE size,
        Window* window_parent
        )->HWND {
            WNDCLASS wc = { 0 };
            wc.lpfnWndProc = DummyWndProc;
            wc.hInstance = g_hInstance->Handle();
            wc.lpszClassName = className;
            wc.hbrBackground = (HBRUSH)GetStockObject(brush);
            wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);

            RegisterClass(&wc);

            auto handle = CreateWindowEx(
                0,
                wc.lpszClassName,
                L"",
                ws | WS_VISIBLE,
                0, 0, size.cx, size.cy,
                Handle(window_parent),
                NULL,
                wc.hInstance,
                NULL
            );
            return handle;
        }(className, brush, ws, size, window_parent)),
        className_(className) {
    }
    ~Window() {
        DestroyWindow(handle_);
    }
    static Window* FromHwnd(HWND hwnd) {
        return reinterpret_cast<Window*>(GetWindowLongPtr(hwnd, GWLP_USERDATA));
    }
    static HWND Handle(Window* window) {
        if (window == nullptr) return NULL;
        return window->handle_;
    }
    static void MessageLoop() {
        MSG msg;
        while (GetMessage(&msg, NULL, 0, 0)) {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
    }
};

class Rgn {
    HRGN rgn_;
public:
	const Rgn& operator=(const Rgn& other) {
        if (this != &other) {
            if (rgn_) DeleteObject(rgn_);
            auto result = CombineRgn(rgn_, other.rgn_, NULL, RGN_COPY);
        }
        return *this;
	}
    const Rgn& operator|=(const Rgn& other) {
        if (this != &other) {
            auto rgn = CreateRectRgn(0, 0, 0, 0);
            auto result = CombineRgn(rgn, this->rgn_, other.rgn_, RGN_OR);
			DeleteObject(this->rgn_);
			rgn_ = rgn;
        }
        return *this;
	}
    HRGN Handle() const { return rgn_; }
    operator HRGN() const {
        return rgn_;
    }
    Rgn(int x, int y, int width, int height) {
        rgn_ = CreateRectRgn(x, y, x + width, y + height);
    }
    Rgn(const Rgn& rgn) {
        rgn_ = CreateRectRgn(0, 0, 0, 0);
        auto result = CombineRgn(rgn_, rgn.rgn_, NULL, RGN_COPY);
		assert(result != ERROR);
    }
    ~Rgn() {
        DeleteObject(rgn_);
    }
};
static Rgn operator|(Rgn rgn1, Rgn rgn2) {
	Rgn result{ rgn1 };
	result |= rgn2;
	return result;
}

class WindowNumber : public Window {
    int number_ = 0;
    void ApplyNumber() {
        auto number = number_;
        number %= 10;
        static uint16_t s_arr_pattern[] = {
            0b111111000111111,
            0b000001111100000,
            0b101111010111101,
            0b111111010110101,
            0b111110010000111,
            0b111011010110111,
            0b111011010111111,
            0b111110000100001,
            0b111111010111111,
            0b111111010110111,
        };
        Rgn rgn{ 0,0,0,0 };
        auto pattern = s_arr_pattern[number];
        for (int x = 0; x < 3; x++) {
            for (int y = 0; y < 5; y++) {
                if (pattern & 1) {
                    rgn |= Rgn{ x * 5,y * 5,5,5 };
                }
                pattern >>= 1;
            }
        }
        SetWindowRgn(Handle(), rgn.Handle(), TRUE);
        DeleteObject(rgn);
    }
public:
    void SetNumber(int number) {
        if (number_ != number) {
            number_ = number;
            ApplyNumber();
        }
    }
    int GetNumber() const { return number_; }
    WindowNumber(
        Window* window_parent
    )
        :Window(
            L"Number",
            WHITE_BRUSH,
            WS_CHILD,
            SIZE{ 0,0 },
            window_parent
        ) {
        CreateRectRgn(0, 0, 0, 0);
        ApplyNumber();
    }
};

class IdxPlayerFromMouse {
    static std::map<HANDLE, int> map_mouse_to_player_;
public:
    static int Get(HANDLE hDevice) {
        auto it = map_mouse_to_player_.find(hDevice);
        if (it != map_mouse_to_player_.end()) {
            return it->second % 2;
        }
        // 新しいマウスデバイスの場合、プレイヤーIDを割り当てる
        int idx_player = static_cast<int>(map_mouse_to_player_.size());
        map_mouse_to_player_[hDevice] = idx_player;
        return idx_player % 2;
    }
    // プレイヤーインデックスを再設定する関数を追加
    static void Set(HANDLE hDevice, int idx_player) {
        map_mouse_to_player_[hDevice] = idx_player;
    }
};
// staticメンバの定義
std::map<HANDLE, int> IdxPlayerFromMouse::map_mouse_to_player_;

class MouseOperate {
    std::array<int, 2> delta_y_{0,0};
    HWND hwnd_;
public:
    void WmInput(HRAWINPUT hRawInput) {
        UINT dwSize;
        GetRawInputData(hRawInput, RID_INPUT, NULL, &dwSize, sizeof(RAWINPUTHEADER));

        BYTE* lpb = new BYTE[dwSize];
        if (lpb) {
            if (GetRawInputData(hRawInput, RID_INPUT, lpb, &dwSize, sizeof(RAWINPUTHEADER)) == dwSize) {
                RAWINPUT* raw = (RAWINPUT*)lpb;
                HANDLE hDevice = raw->header.hDevice;
                if (raw->header.dwType == RIM_TYPEMOUSE) {
                    // 左クリック検出
                    if (raw->data.mouse.usButtonFlags & RI_MOUSE_LEFT_BUTTON_DOWN) {
                        // カーソル位置取得
                        POINT pt;
                        GetCursorPos(&pt);
                        // ウィンドウ座標に変換
                        ScreenToClient(hwnd_, &pt);
                        RECT rc;
                        GetClientRect(hwnd_, &rc);
                        int width = rc.right - rc.left;
                        int idx = (pt.x < width / 2) ? 0 : 1;
                        IdxPlayerFromMouse::Set(hDevice, idx);
                    }
                    // プレイヤーインデックス取得
                    int idx_player = IdxPlayerFromMouse::Get(hDevice);
                    delta_y_[idx_player] += raw->data.mouse.lLastY;
                }
            }
            delete[] lpb;
        }
    }
    int DeltaYPop(int idx_player) { auto delta_y = delta_y_[idx_player]; delta_y_[idx_player] = 0; return delta_y; }
    void SetMainWindow(HWND hwnd) {
        hwnd_ = hwnd;
	}
    MouseOperate(HWND hwnd) : hwnd_(hwnd) {
        RAWINPUTDEVICE rid;
        rid.usUsagePage = 0x01;
        rid.usUsage = 0x02;
        rid.dwFlags = RIDEV_INPUTSINK;
        rid.hwndTarget = hwnd;
        RegisterRawInputDevices(&rid, 1, sizeof(rid));
    }
};

struct Coroutine {
    struct promise_type {
        Coroutine get_return_object() { return Coroutine{ std::coroutine_handle<promise_type>::from_promise(*this) }; }
        std::suspend_never initial_suspend() { return {}; }
        std::suspend_never final_suspend() noexcept { return {}; }
        void return_void() {}
        void unhandled_exception() { std::terminate(); }
    };
    std::coroutine_handle<promise_type> handle_;
    Coroutine& operator=(Coroutine&& other) noexcept {
        if (this != &other) {
            if (handle_) handle_.destroy();
            handle_ = other.handle_;
            other.handle_ = nullptr;
        }
        return *this;
    }
    Coroutine(Coroutine&& other) noexcept : handle_(other.handle_) {
        other.handle_ = nullptr;
    }
    Coroutine(std::coroutine_handle<promise_type> h) : handle_(h) {
    }
    ~Coroutine() { if (handle_) handle_.destroy(); }
    bool resume() {
        if (!handle_) {
            return false;
        }
        if (handle_.done()) {
            return false;
        }
        handle_.resume();
        if (handle_.done()) {
            return false;
        }
        return true;
    }
};


LRESULT CALLBACK WindowProcFrame(
    HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam
) {
    static POINT ptPrev = { 0, 0 };
    static BOOL dragging = FALSE;

    switch (uMsg) {
    case WM_KEYDOWN:
        if (wParam == VK_ESCAPE) {
            auto window = Window::FromHwnd(hwnd);
            if (window) {
                window->ShouldClose(true);
            }
            return 0;
        }
        break;
    case WM_LBUTTONDOWN:
        dragging = TRUE;
        ptPrev.x = LOWORD(lParam);
        ptPrev.y = HIWORD(lParam);
        SetCapture(hwnd);
        return 0;

    case WM_MOUSEMOVE:
        if (dragging) {
            POINT ptCurr = { LOWORD(lParam), HIWORD(lParam) };
            RECT rect;
            GetWindowRect(hwnd, &rect);
            MoveWindow(hwnd, rect.left + ptCurr.x - ptPrev.x, rect.top + ptCurr.y - ptPrev.y,
                rect.right - rect.left, rect.bottom - rect.top, TRUE);
        }
        return 0;

    case WM_LBUTTONUP:
        dragging = FALSE;
        ReleaseCapture();
        return 0;

    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;
    }

    return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

class WindowMouseOperate : public Window {
    std::unique_ptr<MouseOperate> mouse_operate_;
    static LRESULT WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
        auto window = Window::FromHwnd(hwnd);
        auto window_mouse_operate =
            reinterpret_cast<WindowMouseOperate*>(
                window
                );
        auto mouse_operate = window_mouse_operate ? window_mouse_operate->mouse_operate_.get() : nullptr;
        switch (uMsg) {
        case WM_INPUT:
            if (mouse_operate) mouse_operate->WmInput((HRAWINPUT)lParam);
            return 0;
        case WM_DESTROY:
            PostQuitMessage(0);
            break;
        }
        return DefWindowProc(hwnd, uMsg, wParam, lParam);
    };
public:
    MouseOperate* GetMouseOperate() const {
        return mouse_operate_.get();
    }
    void SetMainWindow(HWND hwnd) {
        if (mouse_operate_) {
            mouse_operate_->SetMainWindow(hwnd);
        }
	}
    WindowMouseOperate() :
        Window(
            L"MouseOperate",
            BLACK_BRUSH,
            WS_POPUP,
            SIZE{ 0,0 },
            NULL
        ),
        mouse_operate_(new MouseOperate(Handle())) {
        SetWindowProc(WndProc);
    }
};
template<auto coroutine>
class WindowWithCoroutine : public Window {
    using CoroutinePtr = std::unique_ptr<Coroutine, void(*)(Coroutine*)>;
    mutable CoroutinePtr coroutine_;
    static LRESULT CALLBACK WindowProc(
        HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam
    ) {
        auto window = Window::FromHwnd(hwnd);
        auto window_with_coroutine =
            reinterpret_cast<WindowWithCoroutine*>(
                window
                );
        switch (uMsg) {
        case WM_TIMER:;
            if (window_with_coroutine == nullptr) {
                return 0;
            }
            if (window_with_coroutine->Step()) {
                return 0;
            }
            KillTimer(hwnd, TIMER_FRAME);
            return 0;

        case WM_DESTROY:
            PostQuitMessage(0);
            break;
        }
        return DefWindowProc(hwnd, uMsg, wParam, lParam);
    }
public:
    bool Step() const {
        if (coroutine_) {
            if (coroutine_->resume()) {
                return true;
            }
            coroutine_.reset();
        }
        return false;
    }
    WindowWithCoroutine(const WindowWithCoroutine&) = delete;
    WindowWithCoroutine(
        LPCWSTR className,
        Window* window_parent
    ) : Window(
        className,
        BLACK_BRUSH,
        WS_EX_APPWINDOW,
        SIZE{ 0,0 },
        window_parent
    ),
        coroutine_(new Coroutine(coroutine(this)), [](Coroutine* c) { delete c; })
    {
        Show(SW_HIDE);
        SetTimer(TIMER_FRAME, static_cast<UINT>(1000 / 59.94 + 0.5), NULL);
        SetWindowProc(WindowProc);
    }
};

static Coroutine GameMain(Window* window) {
    BeepControl beep_control;
    WindowMouseOperate window_mouse_operate;
    auto mouse_operate = window_mouse_operate.GetMouseOperate();

    class WindowLines : public Window {
    public:
        WindowLines(
            Window* window_parent
        ) : Window(
            L"Lines",
            WHITE_BRUSH,
            WS_CHILD | WS_VISIBLE,
            Field::GetSize(),
            window_parent
        ) {
            Rgn rgn_line_upper(0, Field::Top()-5, Field::GetWidth(), Field::Top());
            Rgn rgn_line_lower(0, Field::Bottom(), Field::GetWidth(), Field::Bottom()+5);
            auto rgn_lines = rgn_line_upper | rgn_line_lower;
            for (int y = 10; y < Field::GetHeight() - 5; y += 20) {
                Rgn rgn_line(Field::GetWidth() / 2 - 3, y, 6, 10);
                rgn_lines |= rgn_line;
            }
            SetWindowRgn(Handle(), rgn_lines, TRUE);
        }
    };

    class WindowField : public Window {
        WindowLines window_lines_;
    public:

        WindowField(
            Window* window_parent
        )
            :Window(
                L"Field",
                BLACK_BRUSH,
                WS_POPUP,
                //WS_CHILD | WS_VISIBLE,
                Field::GetSize(),
                window_parent
            ),
            window_lines_(this) {
            Rgn rgn(0, 0, Field::GetWidth(), Field::GetHeight());
            SetWindowRgn(Handle(), rgn, TRUE);
            int screen_width = GetSystemMetrics(SM_CXSCREEN);
            int screen_height = GetSystemMetrics(SM_CYSCREEN);
            SetPos(nullptr,
                screen_width / 2 - Field::GetWidth() / 2,
                screen_height / 2 - Field::GetHeight() / 2,
                Field::GetWidth(), Field::GetHeight(),
                SWP_NOZORDER | SWP_SHOWWINDOW
            );
        }
    };
    WindowField window_field{ window };
	window_field.TopMost();
    window_field.SetWindowProc(WindowProcFrame);
	window_mouse_operate.SetMainWindow(window_field.Handle());
    struct ScoreSet {
        int score_{ 0 };
        WindowNumber window_;
        void Apply() {
            window_.SetNumber(score_);
        }
        void operator++() {
            score_++;
            Apply();
		}
        void Reset() {
            score_ = 0;
            Apply();
		}
        ScoreSet(Window* window_parent)
            : window_(window_parent) {
        }
    };
    std::array<ScoreSet, 2> arr_score_set{ &window_field, &window_field };
    for (auto idx_player = 0; idx_player < arr_score_set.size(); idx_player++) {
        arr_score_set[idx_player].window_.MoveWindow(
            (Field::GetWidth() - 20)*idx_player,10,
            30, 50, TRUE
        );
    }
    class WindowPaddle : public Window {
        int x_;
    public:
        WindowPaddle(
            int x,
            Window* window_parent
        ) : Window(
            L"Paddle",
            WHITE_BRUSH,
            WS_CHILD | WS_VISIBLE,
            SIZE{ 0,0 },
            window_parent
        ), x_(x) {
        }
        void Apply(const Paddle& paddle) {
            MoveWindow(x_, paddle.GetY(), paddle.GetSize().cx, paddle.GetSize().cy, TRUE);
        }
    };
    struct PaddleSet {
		int x_; // 当たり判定用のパドルのX座標
        Paddle paddle_;
		WindowPaddle window_paddle_;
        void Apply() {
			window_paddle_.Apply(paddle_);
        }
    };
    std::array<PaddleSet, 2> arr_paddle_set{ {
        {
            Paddle::GetSize().cx,
            { (Field::GetHeight() - Paddle::GetSize().cy) / 2, Field::GetHeight()},
            { 0, &window_field },
        },
        {
			Field::GetWidth() - Paddle::GetSize().cx - Ball::FieldSize().cx, // ボールのサイズを考慮
            { (Field::GetHeight() - Paddle::GetSize().cy) / 2, Field::GetHeight()},
            { Field::GetWidth() - Paddle::GetSize().cx, &window_field },
        }
    }};
    window_field.InvalidateRect(NULL, TRUE);
    window_field.Update();
    while (true) {
        while ((GetAsyncKeyState(VK_LBUTTON) & 0x8000) == 0) {
            if (window_field.ShouldClose()
                || (GetAsyncKeyState(VK_RBUTTON) & 0x8000) != 0) {
                co_return;
            }
            for(int idx_paddle=0; idx_paddle < arr_paddle_set.size(); idx_paddle++) {
                auto& paddle_set = arr_paddle_set[idx_paddle];
                paddle_set.paddle_.Move(mouse_operate->DeltaYPop(idx_paddle));
                paddle_set.Apply();
			}
            co_await std::suspend_always{};
        }
        for(auto &score_set : arr_score_set) {
            score_set.Reset();
		}

        while (
            [&arr_score_set]() ->bool {
                for (auto& score_set : arr_score_set) {
                    if (!(score_set.score_ < 3)) return false;
                }
                return true;
            }()
        ) {
            class WindowBall : public Window {
            public:
                void Apply(const Ball& ball) {
                    MoveWindow(ball.FieldPosition().x, ball.FieldPosition().y, ball.FieldSize().cx, ball.FieldSize().cy, TRUE);
                }
                WindowBall(
                    Window* window_parent
                ) : Window(
                    L"Ball",
                    WHITE_BRUSH,
                    WS_CHILD | WS_VISIBLE,
                    Ball::FieldSize(),
                    window_parent
                ) {
                }
            };
            struct BallSet {
                Ball ball_;
                WindowBall window_ball_;
                void Apply() {
                    window_ball_.Apply(ball_);
                }
                BallSet(Window* window_parent,
                    int velocity)
                    : ball_{
                        POINT{Field::GetWidth() / 2, Field::GetHeight() / 2},
                        velocity
                    }
                    , window_ball_(window_parent) {
                }
            };
            BallSet ball_set{ &window_field, 5 };
            int cnt_bound_paddle = 0;
            do {
                if (window_field.ShouldClose()
                    || (GetAsyncKeyState(VK_RBUTTON) & 0x8000) != 0) {
                    co_return;
                }

                for (int idx_paddle = 0; idx_paddle < arr_paddle_set.size(); idx_paddle++) {
                    auto& paddle_set = arr_paddle_set[idx_paddle];
                    paddle_set.paddle_.Move(mouse_operate->DeltaYPop(idx_paddle));
                    paddle_set.Apply();
                }
                // 画面端チェック
                bool bound = false;
				ball_set.ball_.Move();
				// パドルとの衝突チェック
                if (ball_set.ball_.BoundLeft(
                    arr_paddle_set[0].x_,
                    arr_paddle_set[0].paddle_.GetY(),
                    arr_paddle_set[0].paddle_.GetSize().cy)
                ) {
                    bound = true;
                }
				if (ball_set.ball_.BoundRight(
                    arr_paddle_set[1].x_,
                    arr_paddle_set[1].paddle_.GetY(),
					arr_paddle_set[1].paddle_.GetSize().cy)
                ) {
					bound = true;
				}
                if (bound) {
                    cnt_bound_paddle++;
                    int bits = (cnt_bound_paddle >> 1); // /2
                    if((bits & (bits-1))==0) { // 2の累乗のとき
						// 2, 4, 8, 16, ... のときにボールの速度を上げる
                        ball_set.ball_.SpeedUp();
                    }
                }
                bound = ball_set.ball_.Bound(Field::Top(), Field::Bottom()) || bound;
                ball_set.Apply();
                if (bound) {
                    beep_control.Play();
                }
                co_await std::suspend_always{};
            } while (
                [&ball_set,&arr_score_set]()->bool {
                    if (ball_set.ball_.FieldPosition().x < Field::Left()) {
                        arr_score_set[1].operator++();
                        return false;
                    }
                    if (Field::Right() <= ball_set.ball_.FieldPosition().x + ball_set.ball_.FieldSize().cx) {
                        arr_score_set[0].operator++();
                        return false;
                    }
					return true;
                }()
            );
        }
    }
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) {
    g_hInstance.reset(new HInstance(hInstance));
    auto window_main =
        new WindowWithCoroutine<GameMain>(
            L"Breakout",
            nullptr
        );
    Window::MessageLoop();
    return 0;
}
